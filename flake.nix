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

    emacs.url = "github:cmacrae/emacs";

    rnix-lsp = {
      url = "github:nix-community/rnix-lsp";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, darwin, emacs, flake-utils, nixpkgs, ... }@inputs:
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
          # Auto upgrade nix package and the daemon service.
          maxJobs = "auto";
          buildCores = 4;
          gc = {
            automatic = true;
            options = "--delete-older-than 3d";
          };
        };

        environment.systemPackages = with pkgs; [
          rnix-lsp
        ];

        # TODO: come back to this once ssh is working so we can access the private pragmatapro repo
        fonts = {
          enableFontDir = true;
          fonts = with pkgs; [
            ibm-plex
            inter
            pragmatapro
            public-sans
          ];
        };

        nixpkgs = {
          config = { allowUnfree = true; };
          overlays = [
            emacs.overlay
            self.overlays
          ];
        };

        time.timeZone = config.my.timezone;
      };

      # TODO: come back to this fancy stuff later (maybe?)
      # https://github.com/hlissner/dotfiles/blob/master/flake.nix
      # lib = nixpkgs.lib.extend
      #   (self: super: { my = import ./lib { inherit pkgs inputs; lib = self; }; });

    in
      {
        overlays = (
          self: super: {
            pragmatapro = (super.callPackage ./dotfield/pkgs/pragmatapro.nix {});

            # https://github.com/NixOS/nixpkgs/pull/108861#issuecomment-832087889
            yabai = super.yabai.overrideAttrs (
              o: rec {
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
              }
            );
          }
        );

        darwinConfigurations = {
          "hodgepodge" = inputs.darwin.lib.darwinSystem {
            inputs = inputs;
            modules = [
              inputs.home-manager.darwinModules.home-manager
              ./dotfield/modules
              sharedHostsConfig
              ./dotfield/hosts/hodgepodge.nix
            ];
          };

          "alleymon" = inputs.darwin.lib.darwinSystem {
            inputs = inputs;
            modules = [
              inputs.home-manager.darwinModules.home-manager
              ./dotfield/modules
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
