{
  description = "Dotfield";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nur.url = "github:nix-community/NUR";
    emacs.url = "github:cmacrae/emacs";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    firefox-addons = {
      url = "gitlab:montchr/nur-expressions/develop?dir=pkgs/firefox-addons";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    firefox-lepton = {
      url = "github:black7375/Firefox-UI-Fix";
      flake = false;
    };

    base16-kitty = {
      url = "github:kdrag0n/base16-kitty";
      flake = false;
    };

    rnix-lsp = {
      url = "github:nix-community/rnix-lsp";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self
    , darwin
    , emacs
    , flake-utils
    , nixpkgs
    , nur
    , ...
    }@inputs:
    let
      sharedHostsConfig = { config, pkgs, lib, options, ... }: {
        nix = {
          package = pkgs.nixFlakes;
          extraOptions = "experimental-features = nix-command flakes";
          binaryCaches = [
            "https://cachix.org/api/v1/cache/dotfield"
            "https://cachix.org/api/v1/cache/emacs"
            "https://cachix.org/api/v1/cache/nix-community"
          ];
          binaryCachePublicKeys = [
            "dotfield.cachix.org-1:b5H/ucY/9PDARWG9uWA87ZKWUBU+hnfF30amwiXiaNk="
            "emacs.cachix.org-1:b1SMJNLY/mZF6GxQE+eDBeps7WnkT0Po55TAyzwOxTY="
            "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          ];
          maxJobs = "auto";
          buildCores = 4;
          gc = {
            automatic = true;
            options = "--delete-older-than 3d";
          };
        };

        networking = {
          # Use Cloudflare DNS
          # https://developers.cloudflare.com/1.1.1.1/
          dns = [
            "1.1.1.1"
            "1.0.0.1"
            "2606:4700:4700::1111"
            "2606:4700:4700::1001"
          ];
        };

        fonts = {
          enableFontDir = true;
          fonts = with pkgs; [ ibm-plex inter pragmatapro public-sans ];
        };

        nixpkgs = {
          config.allowUnfree = true;
          overlays = with inputs; [
            (import ./overlays/yabai.nix)
            emacs.overlay
            nur.overlay
            self.overlays
          ];
        };

        time.timeZone = config.my.timezone;

        environment.systemPackages = with pkgs; [
          (writeScriptBin "dotfield"
            (builtins.readFile ./bin/dotfield))
          yarn
        ];
      };

      sharedDarwinModules =
        let
          nur-no-pkgs = import nur {
            nurpkgs =
              import nixpkgs { system = inputs.flake-utils.lib.defaultSystems; };
          };
        in
        [
          inputs.home-manager.darwinModules.home-manager
          ./modules
          sharedHostsConfig
        ];

    in
    {
      overlays = (final: prev: {
        nix-direnv = (prev.nix-direnv.override { enableFlakes = true; });
        pragmatapro = (prev.callPackage ./pkgs/pragmatapro.nix { });
      });

      darwinConfigurations = {
        HodgePodge = inputs.darwin.lib.darwinSystem {
          inputs = inputs;
          modules = sharedDarwinModules ++ [ ./hosts/hodgepodge.nix ];
          system = "x86_64-darwin";
        };

        alleymon = inputs.darwin.lib.darwinSystem {
          inputs = inputs;
          modules = sharedDarwinModules ++ [ ./hosts/alleymon.nix ];
          system = "x86_64-darwin";
        };
      };

      # for convenience
      # nix build './#darwinConfigurations.hodgepodge.system'
      # vs
      # nix build './#HodgePodge'
      # Move them to `outputs.packages.<system>.name`
      HodgePodge = self.darwinConfigurations.HodgePodge.system;
      alleymon = self.darwinConfigurations.alleymon.system;

    };
}
