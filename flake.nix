{
  description = "Dotfield";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nur.url = "github:nix-community/NUR";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs.url = "github:cmacrae/emacs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    rnix-lsp = {
      url = "github:nix-community/rnix-lsp";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, darwin, emacs, emacs-overlay, flake-utils, nixpkgs, nur, ...
    }@inputs:
    let
      sharedHostsConfig = { config, pkgs, lib, options, ... }: {
        nix = {
          package = pkgs.nixFlakes;
          extraOptions = "experimental-features = nix-command flakes";
          binaryCaches = [
            "https://cachix.org/api/v1/cache/nix-community"
            "https://cachix.org/api/v1/cache/emacs"
          ];
          binaryCachePublicKeys = [
            "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
            "emacs.cachix.org-1:b1SMJNLY/mZF6GxQE+eDBeps7WnkT0Po55TAyzwOxTY="
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
          config = { allowUnfree = true; };
          overlays = [
            (import ./dotfield/overlays/yabai.nix)
            emacs.overlay
            emacs-overlay.overlay
            nur.overlay
            self.overlays
          ];
        };

        time.timeZone = config.my.timezone;

        environment.systemPackages = with pkgs; [
          (writeScriptBin "dotfield"
            (builtins.readFile ./dotfield/bin/dotfield))
          commitizen
          yarn
        ];
      };

      sharedDarwinModules = let
        nur-no-pkgs = import nur {
          nurpkgs =
            import nixpkgs { system = inputs.flake-utils.lib.defaultSystems; };
        };
      in [
        inputs.home-manager.darwinModules.home-manager
        ./dotfield/modules
        sharedHostsConfig
      ];

    in {
      overlays = (final: prev: {
        nix-direnv = (prev.nix-direnv.override { enableFlakes = true; });
        pragmatapro = (prev.callPackage ./dotfield/pkgs/pragmatapro.nix { });
      });

      darwinConfigurations = {
        HodgePodge = inputs.darwin.lib.darwinSystem {
          inputs = inputs;
          modules = sharedDarwinModules ++ [ ./dotfield/hosts/hodgepodge.nix ];
        };

        alleymon = inputs.darwin.lib.darwinSystem {
          inputs = inputs;
          modules = sharedDarwinModules ++ [ ./dotfield/hosts/alleymon.nix ];
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
