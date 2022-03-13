{
  description = "Dotfield";

  inputs = {
    # Channels
    nixos-stable.url = "github:NixOS/nixpkgs/release-21.11";
    nixpkgs-trunk.url = "github:NixOS/nixpkgs/master";
    nixpkgs-darwin-stable.url = "github:NixOS/nixpkgs/nixpkgs-21.11-darwin";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    # Environment/system management.
    darwin.url = "github:montchr/nix-darwin/add-toplevel-option-lib";
    darwin.inputs.nixpkgs.follows = "nixpkgs-darwin-stable";
    home-manager.url = "github:montchr/home-manager/trunk";
    home-manager.inputs.nixpkgs.follows = "nixpkgs-unstable";

    # Flake utilities.
    digga.url = "github:divnix/digga/darwin-support";
    digga.inputs.darwin.follows = "darwin";
    digga.inputs.home-manager.follows = "home-manager";
    utils.url = "github:gytis-ivaskevicius/flake-utils-plus";
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };

    # Sources management.
    nur.url = "github:nix-community/NUR";
    nvfetcher.url = "github:berberman/nvfetcher";

    # Secrets management.
    agenix.url = "github:montchr/agenix/trunk";
    agenix.inputs.nixpkgs.follows = "nixos-stable";
    agenix-cli.url = "github:cole-h/agenix-cli";

    # Development tools.
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    rnix-lsp.url = "github:nix-community/rnix-lsp";

    # Other sources.
    prefmanager.url = "github:malob/prefmanager";
    prefmanager.inputs.nixpkgs.follows = "nixpkgs-unstable";
    base16-kitty = { url = "github:kdrag0n/base16-kitty"; flake = false; };
    firefox-lepton = { url = "github:black7375/Firefox-UI-Fix"; flake = false; };
    nix-colors.url = "github:montchr/nix-colors";

    nixpkgs.follows = "nixos-stable";
  };

  outputs =
    { self
    , agenix
    , darwin
    , digga
    , emacs-overlay
    , home-manager
    , nix-colors
    , nixos-stable
    , nixpkgs
    , nixpkgs-unstable
    , nur
    , nvfetcher
    , utils
    , ...
    } @ inputs:
    let

      nixlib = nixpkgs-unstable.lib;

      importables = rec {
        profiles = {
          system = digga.lib.rakeLeaves ./profiles // {
            users = digga.lib.rakeLeaves ./users;
          };
          home = digga.lib.rakeLeaves ./users/profiles;
        };

        suites = with profiles; rec {
          base = [
            system.core
            home.bat
            home.git
            home.ranger
          ];
          networking = [
            system.networking.common
          ];
          linux-minimal = suites.base ++ [
            system.linux
            system.users.nixos
            system.users.root
          ];
          nixos = suites.base ++ [
            system.linux
            system.nixos
          ];
          darwin-minimal = suites.base ++ [
            system.darwin.common
          ];
          darwin-gui = suites.base ++ suites.gui ++ [
            system.darwin.common
            system.darwin.system-defaults
            home.darwin.gui
          ];
          developer = suites.base ++ [
            system.languages.nodejs
            home.direnv
            home.languages.nodejs
            home.vim
          ];
          gui = [
            system.fonts
            home.browsers.firefox
            home.espanso
            home.kitty
          ];
          personal = [
            system.security.gnupg
            system.security.yubikey
            system.secrets
            system.users.primary-user
            home.gnupg
            home.mail
            home.pass
            home.rclone
            home.security.yubikey
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
          imports = [ (digga.lib.importOverlays ./overlays/nixos-stable) ];
        };
        nixpkgs-trunk = { };
        nixpkgs-darwin-stable = {
          imports = [ (digga.lib.importOverlays ./overlays/nixpkgs-darwin-stable) ];
          overlays = [
            ./pkgs/darwin
          ];
        };
        nixpkgs-unstable = { };
      };

      lib = import ./lib { lib = digga.lib // nixpkgs-unstable.lib; };

      sharedOverlays = [
        (final: prev: {
          __dontExport = true;
          inherit inputs;
          lib = prev.lib.extend (lfinal: lprev: {
            our = self.lib;
          });
        })

        (import ./pkgs)
        agenix.overlay
        emacs-overlay.overlay
        nur.overlay
        nvfetcher.overlay
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
            { lib.our = self.lib; }
            suites.base

            digga.nixosModules.bootstrapIso
            digga.nixosModules.nixConfig
            home-manager.nixosModules.home-manager
            agenix.nixosModules.age
            nix-colors.homeManagerModule
          ];
        };

        imports = [ (digga.lib.importHosts ./hosts/nixos) ];
        hosts = {
          HodgePodge = { };
          seadoom = { };
          ci-ubuntu = { };
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
            { lib.our = self.lib; }
            suites.base

            home-manager.darwinModules.home-manager
            # `nixosModules` is correct, even for darwin
            agenix.nixosModules.age
            nix-colors.homeManagerModule
          ];
        };

        imports = [ (digga.lib.importHosts ./hosts/darwin) ];
        hosts = {
          alleymon = { };
          ci-darwin = { };
        };
      };

      home = {
        imports = [ (digga.lib.importExportableModules ./users/hm/modules) ];
        modules = [ ];
        importables = rec {
          profiles = digga.lib.rakeLeaves ./users/hm/profiles;
          suites = with profiles; rec {
            base = [ shell ];
            dev = [ aws emacs ];
            darwin = [ os-specific.darwin.keyboard ];
          };
        };
        users = {
          nixos = { suites, ... }: {
            imports = suites.base;
          };
          xtallos = { suites, ... }: {
            imports = suites.base;
          };
          montchr = { suites, ... }: {
            # TODO: while it works currently because i only use this username on
            # darwin machines, i might eventually need a way to exclude or
            # nullify the darwin suite for this user on nixos systems.
            imports = suites.base ++ suites.dev ++ suites.darwin;
          };
        };
      };

      # FIXME: this will result in a conflict if a nixos host and a darwin host
      # have the same name. that's a bug that should be fixed within digga.
      homeConfigurations =
        (digga.lib.mkHomeConfigurations self.nixosConfigurations)
        // (digga.lib.mkHomeConfigurations self.darwinConfigurations);
    };
}
