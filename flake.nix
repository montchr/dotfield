{
  description = "Dotfield";
  inputs = {
    # Channels
    nixos-stable.url = "github:NixOS/nixpkgs/nixos-22.05";
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-trunk.url = "github:NixOS/nixpkgs/master";
    nixpkgs-darwin-stable.url = "github:NixOS/nixpkgs/nixpkgs-22.05-darwin";

    # Flake utilities.
    digga.url = "github:divnix/digga/home-manager-22.11";
    digga.inputs.nixpkgs.follows = "nixpkgs";
    digga.inputs.darwin.follows = "darwin";
    digga.inputs.home-manager.follows = "home-manager";
    digga.inputs.home-manager.inputs.nixpkgs.follows = "nixpkgs";
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

    # User environments.
    home-manager.url = "github:montchr/home-manager/trunk";
    # home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixos-unstable";

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
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixos-stable";

    # Development tools.
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    rnix-lsp.url = "github:nix-community/rnix-lsp";

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
    sops-nix,
    ...
  } @ inputs': let
    inputs = inputs' // {
      # FIXME: https://github.com/divnix/digga/issues/464#issuecomment-1154974631
      emacs-overlay = inputs'.emacs-overlay // {
        overlay = self.lib.overlayNullProtector inputs'.emacs-overlay.overlay;
      };
    };
    peers = import ./ops/metadata/peers.nix;
  in
    digga.lib.mkFlake {
      inherit self inputs;

      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"

        # FIXME: many python packages will not build on this system due to
        # broken pyopenssl dependency
        # https://github.com/NixOS/nixpkgs/issues/175875
        # https://github.com/pyca/pyopenssl/issues/873
        # "aarch64-darwin"
      ];

      channelsConfig.allowUnfree = true;

      channels = {
        nixos-stable = {
          imports = [
            (digga.lib.importOverlays ./overlays/common)
            (digga.lib.importOverlays ./overlays/nixos-stable)
          ];
          overlays = [
            inputs.emacs-overlay.overlay
          ];
        };
        nixpkgs-darwin-stable = {
          imports = [
            (digga.lib.importOverlays ./overlays/common)
            (digga.lib.importOverlays ./overlays/nixpkgs-darwin-stable)
          ];
          overlays = [
            ./pkgs/darwin
            inputs.emacs-overlay.overlay
          ];
        };
        nixos-unstable = {
          imports = [
            (digga.lib.importOverlays ./overlays/nixos-unstable)
          ];
        };
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

        agenix.overlay
        gitignore.overlay
        nur.overlay
        nvfetcher.overlay

        (import ./pkgs)
        (import ./pkgs/fonts/iosevka.nix)
      ];

      nixos = {
        hostDefaults = {
          system = "x86_64-linux";
          channelName = "nixos-stable";
          imports = [(digga.lib.importExportableModules ./modules)];
          modules = [
            {lib.our = self.lib;}
            ({suites, ...}: {imports = suites.basic;})
            digga.nixosModules.bootstrapIso
            digga.nixosModules.nixConfig
            home-manager.nixosModules.home-manager
            agenix.nixosModules.age
            sops-nix.nixosModules.sops
          ];
        };

        imports = [(digga.lib.importHosts ./hosts/nixos)];
        hosts = {
          boschic = {};
          hodgepodge = {};
          hierophant = {};
          tsone = {};
          bootstrap-graphical = {};
        };

        importables = rec {
          inherit peers;

          primaryUser = {
            authorizedKeys = import ./identity/authorized-keys.nix;
          };

          profiles =
            digga.lib.rakeLeaves ./profiles
            // {users = digga.lib.rakeLeaves ./users;};

          suites = with profiles; rec {
            basic = [
              core.common
              core.nixos
            ];
            virtual = basic ++ [];
            server =
              basic
              ++ [
                networking.common
                # FIXME: this can cause a lock-out!
                # networking.tailscale
                ssh-host
              ];
            tangible =
              basic
              ++ [
                audio
                bluetooth
                networking.common
                networking.tailscale
                networking.wifi
                printers-scanners
              ];
            workstation =
              tangible
              ++ [
                fonts.common
                fonts.pragmatapro
                gnome-desktop
                secrets
                video
                workstations.common
                yubikey
              ];
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
            ({suites, ...}: {imports = suites.basic;})
            home-manager.darwinModules.home-manager
            # `nixosModules` is correct, even for darwin
            agenix.nixosModules.age
            nix-colors.homeManagerModule
          ];
        };

        imports = [(digga.lib.importHosts ./darwin/machines)];
        hosts = {
          cdotmpln = {};
        };

        importables = rec {
          inherit peers;

          profiles =
            digga.lib.rakeLeaves ./profiles
            // {users = digga.lib.rakeLeaves ./users;};

          suites = with profiles; rec {
            basic = [
              core.common
              core.darwin
              networking.common
              networking.tailscale
            ];
            workstation = [
              fonts.common
              fonts.pragmatapro
              os-specific.darwin.emacs
              os-specific.darwin.gui
              os-specific.darwin.system-defaults
              secrets
              ssh-host
            ];
          };
        };
      };

      home = {
        imports = [(digga.lib.importExportableModules ./users/modules)];
        modules = [
          nix-colors.homeManagerModule
          ({suites, ...}: {imports = suites.basic;})
        ];
        importables = rec {
          inherit peers;
          profiles = digga.lib.rakeLeaves ./users/profiles;
          suites = with profiles; rec {
            #: basic: just your average anybody
            basic = [
              core
              direnv
              misc
              navi
              nnn
              ranger
              secrets.common
              tealdeer
              vim
            ];
            #: server: travellers across the ether
            server = [
              shells.fish
              ssh
            ];
            #: developer: those who go
            developer = [
              direnv
              emacs.doom
              git
              gpg
              shells.zsh
              ssh
            ];
            #: graphical:  familiar personal computing interfaces
            graphical = [
              desktop.common
              desktop.gnome
              firefox
              foot
              keyboard
              kitty
              secrets.one-password
              themes
            ];
            #: listen: hey!
            listen = [apple-music spotify];
            #: workstation: level++
            workstation =
              graphical
              ++ developer
              ++ [
                espanso
                mail
                newsboat
                obs-studio
                secrets.password-store
                sync
                yubikey
              ];
          };
        };

        users = rec {
          nixos = {suites, ...}: {
            imports = [];
          };
          cdom = {suites, ...}: {
            imports = with suites;
              basic ++ developer;
          };
          seadoom = cdom;
          chrismont = {profiles, suites, ...}: {
            imports =
              (with suites; workstation)
              ++ (with profiles; [
                aws
                nodejs
                php
              ]);
          };
        };
      };

      devshell = ./shell;

      homeConfigurations = digga.lib.mkHomeConfigurations self.nixosConfigurations;
        # (digga.lib.collectHosts self.nixosConfigurations self.darwinConfigurations);
        # {
        #   inherit
        #     (self.nixosConfigurations)
        #     boschic
        #     hodgepodge
        #     tsone
        #     ;
        # };

      deploy.nodes = digga.lib.mkDeployNodes self.nixosConfigurations {
        tsone = {
          hostname = peers.hosts.tsone.ipv4.address;
          sshUser = "root";
          fastConnection = true;
          autoRollback = true;
          magicRollback = true;
        };
      };
    };
}
