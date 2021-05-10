{
  description = "Dotfield";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    rnix-lsp = {
      url = "github:nix-community/rnix-lsp";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  # N.B. including nixpkgs here may cause yabai overlay to break!
  outputs = { self, flake-utils, ... }@inputs:
    let
      sharedHostsConfig = { config, pkgs, lib, options, ... }: {
        nix = {
          package = pkgs.nixFlakes;
          extraOptions = "experimental-features = nix-command flakes";
          binaryCaches = [
            "https://cache.nixos.org"
            "https://nix-community.cachix.org"
            "https://nixpkgs.cachix.org"
          ];
          binaryCachePublicKeys = [
            "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
            "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
            "nixpkgs.cachix.org-1:q91R6hxbwFvDqTSDKwDAV4T5PxqXGxswD8vhONFMeOE="
          ];
          # Auto upgrade nix package and the daemon service.
          maxJobs = 4;
          buildCores = 4;
          gc = {
            automatic = true;
            options = "--delete-older-than 3d";
          };
        };

        # TODO: get this working again! but pragmatapro is necessary
        # fonts = {
        #   enableFontDir = true;
        #   fonts = with pkgs; [
        #     ibm-plex
        #     inter
        #     # TODO: come back to this once ssh is working so we can access the private repo
        #     # pragmatapro
        #     public-sans
        #   ];
        # };

        nixpkgs = {
          config = { allowUnfree = true; };
          overlays = [ self.overlays ];
        };

        time.timeZone = config.my.timezone;
      };

      # TODO: come back to this fancy stuff later (maybe?)
      # https://github.com/hlissner/dotfiles/blob/master/flake.nix
      # lib = nixpkgs.lib.extend
      #   (self: super: { my = import ./lib { inherit pkgs inputs; lib = self; }; });

    in {
      overlays = (self: super: {
        # pragmatapro = (super.callPackage ./dotfield/pkgs/pragmatapro.nix { });

        # https://github.com/NixOS/nixpkgs/pull/108861#issuecomment-832087889
        yabai = super.yabai.overrideAttrs (o: rec {
          version = "3.3.8";
          src = builtins.fetchTarball {
            url =
              "https://github.com/koekeishiya/yabai/releases/download/v${version}/yabai-v${version}.tar.gz";
            sha256 = "1qh1vf52j0b3lyrm005c8c98s39rk1lq61rrq0ml2yr4h77rq3xv";
          };

          installPhase = ''
            mkdir -p $out/bin
            mkdir -p $out/share/man/man1/
            cp ./bin/yabai $out/bin/yabai
            cp ./doc/yabai.1 $out/share/man/man1/yabai.1
          '';
        });
      });

      # TODO: SAD.
      # flake-utils.lib.eachDefaultSystem ({
      #   # Nix flakes REPL
      #   # https://github.com/NixOS/nix/issues/3803#issuecomment-748612294
      #   #
      #   # Usage:
      #   #   nix run .#repl
      #   apps.repl = flake-utils.lib.mkApp {
      #     drv = inputs.pkgs.writeShellScriptBin "repl" ''
      #       confnix=$(mktemp)
      #       echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
      #       trap "rm $confnix" EXIT
      #       nix repl $confnix
      #     '';
      #   };
      # });


      darwinConfigurations = {
        "hodgepodge" = inputs.darwin.lib.darwinSystem {
          inputs = inputs;
          modules = [
            inputs.home-manager.darwinModules.home-manager
            ./dotfield/modules/shared
            sharedHostsConfig
            ./dotfield/hosts/hodgepodge.nix
          ];
        };

        "alleymon" = inputs.darwin.lib.darwinSystem {
          inputs = inputs;
          modules = [
            inputs.home-manager.darwinModules.home-manager
            ./dotfield/modules/shared
            sharedHostsConfig
            ./dotfield/hosts/alleymon.nix
          ];
        };
      };

      # for convenience
      # nix build './#darwinConfigurations.hodgepodge.system'
      # vs
      # nix build './#HodgePodge'
      # Move them to `outputs.packages.<system>.name`
      HodgePodge = self.darwinConfigurations.hodgepodge.system;
      alleymon = self.darwinConfigurations.alleymon.system;

    };
}
