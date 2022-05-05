{
  description = "Dotfield";

  inputs = {
    # Channels
    nixos-stable.url = "github:NixOS/nixpkgs/release-21.11";
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-trunk.url = "github:NixOS/nixpkgs/master";
    nixpkgs-darwin-stable.url = "github:NixOS/nixpkgs/nixpkgs-21.11-darwin";

    # Flake utilities.
    digga.url = "github:divnix/digga";
    digga.inputs.nixpkgs.follows = "nixpkgs";
    digga.inputs.darwin.follows = "darwin";
    digga.inputs.home-manager.follows = "home-manager";
    nixlib.url = "github:nix-community/nixpkgs.lib";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    # System management.
    darwin.url = "github:LnL7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs-darwin-stable";
    prefmanager.url = "github:malob/prefmanager";
    prefmanager.inputs.nixpkgs.follows = "nixos-unstable";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nixos-generators.url = "github:nix-community/nixos-generators";
    nixos-generators.inputs.nixlib.follows = "nixlib";
    nixos-generators.inputs.nixpkgs.follows = "nixos-stable";

    # Deployments.
    deploy.url = "github:serokell/deploy-rs";
    deploy.inputs.nixpkgs.follows = "nixpkgs";

    # Sources management.
    nur.url = "github:nix-community/NUR";
    nvfetcher.url = "github:berberman/nvfetcher";
    nvfetcher.inputs.nixpkgs.follows = "nixpkgs";
    gitignore.url = "github:hercules-ci/gitignore.nix";
    gitignore.inputs.nixpkgs.follows = "nixpkgs";

    # Secrets management.
    agenix.url = "github:montchr/agenix/darwin-support";
    agenix.inputs.nixpkgs.follows = "nixpkgs";

    # Development tools.
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    rnix-lsp.url = "github:nix-community/rnix-lsp";
    phps.url = "github:fossar/nix-phps";
    phps.inputs.utils.follows = "digga/flake-utils-plus/flake-utils";
    phps.inputs.nixpkgs.follows = "nixos-unstable";

    # User environments.
    home-manager.url = "github:montchr/home-manager/trunk";
    home-manager.inputs.nixpkgs.follows = "nixos-unstable";

    # Other sources.
    nix-colors.url = "github:Misterio77/nix-colors";
    base16-kitty = {
      url = "github:kdrag0n/base16-kitty";
      flake = false;
    };
    firefox-lepton = {
      url = "github:black7375/Firefox-UI-Fix";
      flake = false;
    };

    nixpkgs.follows = "nixos-stable";
  };

  outputs = {
    self,
    agenix,
    darwin,
    deploy,
    digga,
    emacs-overlay,
    gitignore,
    home-manager,
    nixlib,
    nix-colors,
    nixos-generators,
    nixos-hardware,
    nixos-stable,
    nixpkgs,
    nixos-unstable,
    nur,
    nvfetcher,
    phps,
    ...
  } @ inputs:
    digga.lib.mkFlake {
      inherit self inputs;

      channelsConfig.allowUnfree = true;

      channels = {
        nixos-stable = {
          imports = [
            (digga.lib.importOverlays ./overlays/common)
            (digga.lib.importOverlays ./overlays/nixos-stable)
          ];
        };
        nixpkgs-darwin-stable = {
          imports = [
            (digga.lib.importOverlays ./overlays/common)
            (digga.lib.importOverlays ./overlays/nixpkgs-darwin-stable)
          ];
          overlays = [
            ./pkgs/darwin
          ];
        };
        nixos-unstable = {};
        nixpkgs-trunk = {};
      };

      lib = import ./lib {lib = digga.lib // nixos-unstable.lib;};

      sharedOverlays = [
        (final: prev: {
          __dontExport = true;
          inherit inputs;
          lib = prev.lib.extend (lfinal: lprev: {
            our = self.lib;
          });
        })

        (final: prev: {
          inherit (inputs.phps.packages.${final.system}) php81;
          php = final.php81;
        })

        agenix.overlay
        emacs-overlay.overlay
        gitignore.overlay
        nur.overlay
        nvfetcher.overlay

        (import ./pkgs)
      ];

      nixos = {
        hostDefaults = {
          system = "x86_64-linux";
          channelName = "nixos-stable";
          imports = [(digga.lib.importExportableModules ./modules)];
          modules = [
            {lib.our = self.lib;}
            ({suites, ...}: {imports = suites.base;})
            digga.nixosModules.bootstrapIso
            digga.nixosModules.nixConfig
            home-manager.nixosModules.home-manager
            agenix.nixosModules.age
          ];
        };

        imports = [(digga.lib.importHosts ./hosts/nixos)];
        hosts = {
          HodgePodge = {};
          onceler = {};
          seadoom = {};
          ci-ubuntu = {};
        };

        importables = rec {
          profiles =
            digga.lib.rakeLeaves ./profiles
            // {
              users = digga.lib.rakeLeaves ./users;
            };

          suites = with profiles; rec {
            base = [
              core
              networking.common
              os-specific.nixos
            ];
            minimal =
              base
              ++ [
                users.nixos
                users.root
              ];
            gui = [
              fonts.common
              fonts.pragmatapro
            ];
            personal = [
              secrets
            ];
            tangibles = [networking.wifi];
          };
        };
      };

      darwin = {
        hostDefaults = {
          system = "x86_64-darwin";
          channelName = "nixpkgs-darwin-stable";
          imports = [(digga.lib.importExportableModules ./modules)];
          modules = [
            {lib.our = self.lib;}
            ({suites, ...}: {imports = suites.base;})
            home-manager.darwinModules.home-manager
            # `nixosModules` is correct, even for darwin
            agenix.nixosModules.age
            nix-colors.homeManagerModule
          ];
        };

        imports = [(digga.lib.importHosts ./hosts/darwin)];
        hosts = {
          alleymon = {};
          macOS = {};
          ci-darwin = {};
        };

        importables = rec {
          profiles =
            digga.lib.rakeLeaves ./profiles
            // {
              users = digga.lib.rakeLeaves ./users;
            };

          suites = with profiles; rec {
            base = [core networking.common];
            minimal = base ++ [os-specific.darwin.common];
            gui =
              base
              ++ [
                fonts.common
                fonts.pragmatapro
                os-specific.darwin.common
                os-specific.darwin.gui
                os-specific.darwin.system-defaults
              ];
            standard = base ++ gui ++ [
              os-specific.darwin.emacs
              secrets
            ];
          };
        };
      };

      home = {
        imports = [(digga.lib.importExportableModules ./users/modules)];
        modules = [
          nix-colors.homeManagerModule
          ({suites, ...}: {imports = suites.base;})
        ];
        importables = rec {
          profiles = digga.lib.rakeLeaves ./users/profiles;
          suites = with profiles; rec {
            base = [
              core
              direnv
              git
              misc
              navi
              ranger
              shells.fish
              shells.zsh
              ssh
              tealdeer
            ];
            dev = [
              emacs
              languages.nodejs
              vim
            ];
            gui = [
              colors
              espanso
              firefox
              keyboard
              kitty
            ];
            personal = [
              gnupg
              mail
              secrets
            ];
          };
        };

        users = {
          nixos = {suites, ...}: {
            imports = with suites; base;
          };
          xtallos = {suites, ...}: {
            imports = with suites; base ++ dev ++ personal;
          };
        };
      };

      devshell = ./shell;

      homeConfigurations =
        digga.lib.mkHomeConfigurations
        (digga.lib.collectHosts self.nixosConfigurations self.darwinConfigurations);

      deploy.nodes = digga.lib.mkDeployNodes self.nixosConfigurations {};
    };
}
