{
  description = "Dotfield";

  inputs = {
    # Channels
    nixos-stable.url = "github:NixOS/nixpkgs/release-21.11";
    nixpkgs-trunk.url = "github:NixOS/nixpkgs/master";
    nixpkgs-darwin-stable.url = "github:NixOS/nixpkgs/nixpkgs-21.11-darwin";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    # Environment/system management.
    darwin.url = "github:montchr/nix-darwin/trunk";
    darwin.inputs.nixpkgs.follows = "nixpkgs-darwin-stable";
    home-manager.url = "github:montchr/home-manager/trunk";
    home-manager.inputs.nixpkgs.follows = "nixpkgs-unstable";

    # Flake utilities.
    digga.url = "github:divnix/digga/darwin-support";
    digga.inputs.nixpkgs.follows = "nixpkgs";
    digga.inputs.darwin.follows = "darwin";
    digga.inputs.home-manager.follows = "home-manager";
    utils.url = "github:gytis-ivaskevicius/flake-utils-plus";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    # Sources management.
    nur.url = "github:nix-community/NUR";
    gitignore.url = "github:hercules-ci/gitignore.nix";
    gitignore.inputs.nixpkgs.follows = "nixpkgs";

    # Secrets management.
    agenix.url = "github:montchr/agenix/trunk";
    agenix.inputs.nixpkgs.follows = "nixos-stable";
    agenix-cli.url = "github:cole-h/agenix-cli";

    # Development tools.
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    rnix-lsp.url = "github:nix-community/rnix-lsp";

    # Other sources.
    nix-colors.url = "github:Misterio77/nix-colors";
    prefmanager.url = "github:malob/prefmanager";
    prefmanager.inputs.nixpkgs.follows = "nixpkgs-unstable";
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
    digga,
    emacs-overlay,
    gitignore,
    home-manager,
    nix-colors,
    nixos-stable,
    nixpkgs,
    nixpkgs-unstable,
    nur,
    utils,
    ...
  } @ inputs: let
    nixlib = nixpkgs-unstable.lib;

    importables = rec {
      profiles = {
        system =
          digga.lib.rakeLeaves ./profiles
          // {
            users = digga.lib.rakeLeaves ./users;
          };
        home = digga.lib.rakeLeaves ./users/profiles;
      };

      suites = with profiles; rec {
        base = [
          system.core
        ];
        networking = [
          system.networking.common
        ];
        linux-minimal =
          suites.base
          ++ [
            system.os-specific.linux
            system.users.nixos
            system.users.root
          ];
        nixos =
          suites.base
          ++ [
            system.os-specific.linux
            system.os-specific.nixos
          ];
        darwin-minimal =
          suites.base
          ++ [
            system.os-specific.darwin.common
          ];
        darwin-gui =
          suites.base
          ++ suites.gui
          ++ [
            system.os-specific.darwin.common
            system.os-specific.darwin.gui
            system.os-specific.darwin.system-defaults
          ];
        developer = suites.base ++ [];
        gui = [
          system.fonts
        ];
        personal = [
          system.security.gnupg
          system.secrets
          system.users.primary-user
          home.secrets
          home.ssh
        ];
        work = [
          system.languages.php
          system.languages.ruby # for vagrant
          system.virtualbox
        ];
      };
    };
  in
    digga.lib.mkFlake {
      inherit self inputs;

      channelsConfig.allowUnfree = true;

      channels = {
        nixos-stable = {
          imports = [(digga.lib.importOverlays ./overlays/nixos-stable)];
        };
        nixpkgs-trunk = {};
        nixpkgs-darwin-stable = {
          imports = [(digga.lib.importOverlays ./overlays/nixpkgs-darwin-stable)];
          overlays = [
            ./pkgs/darwin
          ];
        };
        nixpkgs-unstable = {};
      };

      lib = import ./lib {lib = digga.lib // nixpkgs-unstable.lib;};

      sharedOverlays = [
        (final: prev: {
          __dontExport = true;
          inherit inputs;
          lib = prev.lib.extend (lfinal: lprev: {
            our = self.lib;
          });
        })

        agenix.overlay
        emacs-overlay.overlay
        gitignore.overlay
        nur.overlay

        (import ./pkgs)
      ];

      nixos = {
        inherit importables;

        hostDefaults = {
          system = "x86_64-linux";
          channelName = "nixos-stable";
          imports = [
            (digga.lib.importExportableModules ./modules)
            (digga.lib.importExportableModules ./users/modules)
          ];
          modules = with importables; [
            {lib.our = self.lib;}

            suites.base

            digga.nixosModules.bootstrapIso
            digga.nixosModules.nixConfig
            home-manager.nixosModules.home-manager
            agenix.nixosModules.age
          ];
        };

        imports = [(digga.lib.importHosts ./hosts/nixos)];
        hosts = {
          HodgePodge = {};
          seadoom = {};
          ci-ubuntu = {};
        };
      };

      darwin = {
        inherit importables;

        hostDefaults = {
          channelName = "nixpkgs-darwin-stable";
          imports = [
            (digga.lib.importExportableModules ./modules)
            (digga.lib.importExportableModules ./users/modules)
          ];
          modules = with importables; [
            {lib.our = self.lib;}
            suites.base

            home-manager.darwinModules.home-manager
            # `nixosModules` is correct, even for darwin
            agenix.nixosModules.age
            nix-colors.homeManagerModule
          ];
        };

        imports = [(digga.lib.importHosts ./hosts/darwin)];
        hosts = {
          alleymon = {};
          ci-darwin = {};
        };
      };

      home = {
        imports = [(digga.lib.importExportableModules ./users/hm/modules)];
        modules = [
          nix-colors.homeManagerModule
        ];
        importables = rec {
          profiles = digga.lib.rakeLeaves ./users/hm/profiles;
          suites = with profiles; rec {
            base = [
              bat
              direnv
              git
              ranger
              shell
              tealdeer
            ];
            dev = [
              aws
              emacs
              languages.nodejs
              vim
            ];
            gui = [
              espanso
              firefox
              graphical.colors
              kitty
            ];
            darwin = [
              os-specific.darwin.keyboard
            ];
            personal =
              suites.base
              ++ suites.dev
              ++ [gnupg mail secrets];
          };
        };

        users = {
          nixos = {suites, ...}: {
            imports = suites.base;
          };
          xtallos = {suites, ...}: {
            imports = suites.personal;
          };
        };
      };

      # FIXME: this may be causing the evaluation of host configurations on the
      # wrong platforms. darwinConfigurations cannot(?) be used on nixos, and
      # vice versa.
      #
      # homeConfigurations =
      #   (digga.lib.mkHomeConfigurations self.nixosConfigurations)
      #   // (digga.lib.mkHomeConfigurations self.darwinConfigurations);
    };
}
