{
  description = "Dotfield";

  nixConfig.extra-experimental-features = "nix-command flakes";
  nixConfig.extra-substituters = "https://dotfield.cachix.org https://nix-community.cachix.org";
  nixConfig.extra-trusted-public-keys = "dotfield.cachix.org-1:b5H/ucY/9PDARWG9uWA87ZKWUBU+hnfF30amwiXiaNk= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=";

  inputs = {
    nixos-stable.url = "github:NixOS/nixpkgs/nixos-22.05";
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-trunk.url = "github:NixOS/nixpkgs/master";
    nixpkgs-darwin-stable.url = "github:NixOS/nixpkgs/nixpkgs-22.05-darwin";
    nixos-unstable-iosevka-185633.url = "github:NixOS/nixpkgs?rev=12363fb6d89859a37cd7e27f85288599f13e49d9";
    nixlib.url = "github:nix-community/nixpkgs.lib";

    ##: --- utilities ----------------------------------------------------------

    agenix.url = "github:montchr/agenix/darwin-support";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    darwin.url = "github:LnL7/nix-darwin";
    deploy.url = "github:serokell/deploy-rs";
    digga.url = "github:divnix/digga";
    flake-utils.url = "github:numtide/flake-utils";
    prefmanager.url = "github:malob/prefmanager";
    nixos-generators.url = "github:nix-community/nixos-generators";
    nvfetcher.url = "github:berberman/nvfetcher";
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixos-stable";

    home-manager = {
      # url = "github:montchr/home-manager/trunk";
      url = "github:nix-community/home-manager/release-22.05";
      inputs.nixpkgs.follows = "nixos-unstable";
    };

    ##: --- sources ------------------------------------------------------------

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    gitignore.url = "github:hercules-ci/gitignore.nix";
    nix-colors.url = "github:Misterio77/nix-colors";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nur.url = "github:nix-community/NUR";
    rnix-lsp.url = "github:nix-community/rnix-lsp";

    base16-kitty = {
      url = "github:kdrag0n/base16-kitty";
      flake = false;
    };

    firefox-lepton = {
      url = "github:black7375/Firefox-UI-Fix";
      flake = false;
    };

    nixpkgs-wayland = {
      url = "github:nix-community/nixpkgs-wayland";
      # Packages are built against this channel.
      inputs.nixpkgs.follows = "nixos-unstable";
      inputs.master.follows = "nixpkgs-trunk";
    };

    ##: --- other --------------------------------------------------------------

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    nixpkgs.follows = "nixos-stable";
  };

  outputs = {
    self,
    nixpkgs,
    agenix,
    darwin,
    deploy,
    digga,
    emacs-overlay,
    flake-utils,
    gitignore,
    home-manager,
    nixlib,
    nix-colors,
    nixos-generators,
    nixos-hardware,
    nixos-stable,
    nixos-unstable,
    nixpkgs-wayland,
    nur,
    nvfetcher,
    sops-nix,
    ...
  } @ inputs: let
    inherit
      (digga.lib)
      flattenTree
      rakeLeaves
      ;
    inherit
      (flake-utils.lib)
      eachSystem
      system
      ;

    supportedSystems = with system; [
      x86_64-linux
      x86_64-darwin

      # FIXME: Something in this flake's chain of dependencies triggers a build
      # failure when `aarch64-darwin` is added to `supportedSystems`,
      # specifically due to `pyopenssl`. Many python packages will not build on
      # this system due to the broken `pyopenssl` dependency.
      # [Updated: 2022-08-08]
      # https://github.com/NixOS/nixpkgs/issues/175875
      # https://github.com/pyca/pyopenssl/issues/873
      # aarch64-darwin
    ];

    darwinSystems = [system.x86_64-darwin system.aarch64-darwin];
    peers = import ./ops/metadata/peers.nix;
    overlays = [
      agenix.overlay
      emacs-overlay.overlay
      gitignore.overlay
      nixpkgs-wayland.overlay
      nur.overlay
      nvfetcher.overlay
    ];
  in
    (digga.lib.mkFlake {
      inherit
        self
        inputs
        supportedSystems
        ;

      channelsConfig.allowUnfree = true;

      channels = {
        nixos-stable = {
          inherit overlays;
          imports = [
            (digga.lib.importOverlays ./overlays/stable)
            (digga.lib.importOverlays ./packages)
          ];
        };
        nixpkgs-darwin-stable = {
          imports = [
            (digga.lib.importOverlays ./overlays/stable)
            (digga.lib.importOverlays ./packages)
          ];
          overlays =
            overlays
            ++ [
              (final: prev: {yabai = self.packages.${final.system}.yabai;})
            ];
        };
        nixos-unstable = {
          inherit overlays;
          imports = [
            (digga.lib.importOverlays ./overlays/nixos-unstable)
          ];
        };
        nixpkgs-trunk = {};
      };

      lib = import ./lib {
        inherit peers;
        lib = digga.lib // nixos-unstable.lib;
      };

      sharedOverlays = [
        (final: prev: {
          __dontExport = true;
          inherit inputs;
          lib = prev.lib.extend (lfinal: lprev: {
            our = self.lib;
          });
        })
      ];

      nixos = {
        hostDefaults = {
          system = "x86_64-linux";
          channelName = "nixos-stable";
          imports = [(digga.lib.importExportableModules ./modules)];
          modules = [
            # TODO: can this be merged with the 'dotfield' lib?
            {lib.our = self.lib;}
            ({suites, ...}: {imports = suites.basic ++ [./lib/system];})
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
            authorizedKeys = import ./secrets/authorized-keys.nix;
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
                # FIXME: this can cause a lock-out! needs a one-shot systemd
                # unit to activate first. see Xe's articles/config
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
                zoom-us
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
            ({suites, ...}: {imports = suites.basic ++ [./lib/system];})
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
          ({suites, ...}: {imports = suites.basic ++ [./lib/home];})
        ];
        importables = rec {
          inherit peers;
          profiles = digga.lib.rakeLeaves ./users/profiles;
          suites = with profiles; rec {
            #: basic: just your average anybody
            basic = [
              core
              direnv
              # FIXME: many of the packages in `misc` should only be added to
              # graphical or media-workflow environments (e.g. ffmpeg)
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
              dhall
              direnv
              emacs
              git
              gpg
              python
              shells.zsh
              shells.fish
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
              mpv
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
                promnesia
                secrets.password-store
                sync
                yubikey
                zotero
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
          "cdom@dev.klein.temple.edu" = {suites, ...}: {
            imports = with suites;
              basic ++ developer ++ server;
          };
          chrismont = {
            profiles,
            suites,
            ...
          }: {
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

      deploy.nodes = digga.lib.mkDeployNodes self.nixosConfigurations {
        tsone = {
          hostname = peers.hosts.tsone.ipv4.address;
          sshUser = "root";
          fastConnection = true;
          autoRollback = true;
          magicRollback = true;
        };
      };
    })
    // (eachSystem darwinSystems (system: {
      packages =
        builtins.mapAttrs (n: v: nixpkgs.legacyPackages.${system}.callPackage v {})
        (flattenTree (rakeLeaves ./darwin/packages));
    }));
}
