{
  description = "Dotfield";

  inputs = {
    # Package sets
    nixos-stable.url = "github:NixOS/nixpkgs/release-21.11";
    nixpkgs-trunk.url = "github:NixOS/nixpkgs/master";
    nixpkgs-darwin-stable.url = "github:NixOS/nixpkgs/nixpkgs-21.11-darwin";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    # Environment/system management.
    darwin.url = "github:montchr/nix-darwin/trunk";
    darwin.inputs.nixpkgs.follows = "nixpkgs-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs-unstable";

    # Flake utilities.
    digga.url = "github:divnix/digga";
    digga.inputs.nixpkgs.follows = "nixos-stable";
    digga.inputs.nixlib.follows = "nixos-stable";
    digga.inputs.home-manager.follows = "home-manager";
    utils.url = "github:gytis-ivaskevicius/flake-utils-plus";
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };

    # Sources management.
    nur.url = "github:nix-community/NUR";
    nvfetcher.url = "github:berberman/nvfetcher";

    # Secrets management.
    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixos-stable";
    agenix-cli.url = "github:montchr/agenix-cli/develop";

    # Development tools.
    emacs.url = "github:montchr/emacs/develop";
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
    , emacs
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
            # users = {
            #   montchr = import ./users/primary-user {};
            # };
          };
          home = digga.lib.rakeLeaves ./users/profiles;
        };

        suites = with profiles; rec {
          base = [
            system.core
            home.bash
            home.bat
            home.git
            home.zsh
          ];
          networking = [
            system.networking.common
          ];
          linux-minimal = suites.base ++ [
            system.linux
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
            home.darwin.keyboard
          ];
          developer = suites.base ++ [
            system.languages.nodejs
            home.direnv
            home.emacs
            home.languages.nodejs
          ];
          gui = [
            system.fonts
            home.browsers.firefox
            home.espanso
            home.kitty
          ];
          personal = [
            system.security.yubikey
            system.secrets
            home.gnupg
            home.mail
            home.pass
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

      hostConfigs = digga.lib.rakeLeaves ./hosts;

      mkHosts = hosts: (builtins.foldl' (a: b: a // b) { } hosts);

      mkNixosHost = name:
        { system ? "x86_64-linux"
        , channelName ? "nixos-stable"
        }: {
          ${name} = {
            inherit system channelName;
            modules = with importables;
              suites.base
              ++ [
                hostConfigs.${name}
                home-manager.nixosModules.home-manager
                agenix.nixosModules.age
              ];
          };
        };

      mkDarwinHost = name:
        { system ? "x86_64-darwin"
        , channelName ? "nixpkgs-darwin-stable"
        }: {
          ${name} = {
            inherit system channelName;
            output = "darwinConfigurations";
            builder = darwin.lib.darwinSystem;
            modules = with importables;
              suites.base
              ++ [
                hostConfigs.${name}
                home-manager.darwinModules.home-manager
              ];
          };
        };

      # https://github.com/kclejeune/system/blob/71c65173e7eba8765a3962df5b52c2f2c25a8fac/flake.nix#L89-L109
      # generate a home-manager configuration usable on any unix system
      # with overlays and any extraModules applied
      # mkHomeConfig =
      #   { username
      #   , system ? "x86_64-linux"
      #   , nixpkgs ? inputs.nixpkgs
      #   , stable ? inputs.nixos-stable
      #   , lib ? (mkLib nixpkgs)
      #   , baseModules ? [
      #       ./modules/home-manager
      #       {
      #         home.sessionVariables = {
      #           NIX_PATH =
      #             "nixpkgs=${nixpkgs}:stable=${stable}:trunk=${inputs.trunk}\${NIX_PATH:+:}$NIX_PATH";
      #         };
      #       }
      #     ]
      #   , extraModules ? [ ]
      #   }:
      #   homeManagerConfiguration rec {
      #     inherit system username;
      #     homeDirectory = "${homePrefix system}/${username}";
      #     extraSpecialArgs = { inherit inputs lib nixpkgs stable; };
      #     configuration = {
      #       imports = baseModules ++ extraModules ++ [ ./modules/overlays.nix ];
      #     };
      #   };

    in

    utils.lib.mkFlake {
      inherit self inputs;

      channelsConfig.allowUnfree = true;

      channels = {
        nixos-stable = {
          overlaysBuilder = (channels: [
            (final: prev: {
              inherit (channels.nixpkgs-unstable) nix nix_2_5 nixUnstable;
            })
          ]);
        };
        nixpkgs-trunk = { };
        nixpkgs-darwin-stable = {
          overlaysBuilder = (channels: [
            (import ./pkgs/darwin { inherit self inputs; })
            (final: prev: {
              inherit (channels.nixpkgs-unstable)
                direnv
                nix
                nix_2_5
                nixUnstable
                nix-direnv
                ;
            })
            emacs.overlay
          ]);
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
        nur.overlay
        nvfetcher.overlay
      ];

      hostDefaults = {
        extraArgs = { inherit utils inputs; };
        specialArgs = with importables; { inherit profiles suites; };
        modules = [
          ./users/primary-user
          nix-colors.homeManagerModule
        ] ++ (builtins.attrValues (digga.lib.flattenTree
          (digga.lib.rakeLeaves ./modules)))
        ++ (builtins.attrValues (digga.lib.flattenTree
          (digga.lib.rakeLeaves ./users/modules)));
      };

      hosts = mkHosts [
        (mkDarwinHost "HodgePodge" { })
        (mkDarwinHost "alleymon" { })

        # CI runner hosts.
        (mkDarwinHost "ci-darwin" { })
        (mkNixosHost "ci-ubuntu" { })
      ];

      # https://github.com/kclejeune/system/blob/71c65173e7eba8765a3962df5b52c2f2c25a8fac/flake.nix#L111-L129
      # checks = nixlib.listToAttrs (

      #   # darwin checks
      #   (map
      #     (system: {
      #       name = system;
      #       value = {
      #         darwin =
      #           self.darwinConfigurations.alleymon.config.system.build.toplevel;
      #         # darwinServer =
      #         #   self.homeConfigurations.darwinServer.activationPackage;
      #       };
      #     })
      #     nixlib.platforms.darwin)
      #   # ++

      #   # # linux checks
      #   # (map
      #   #   (system: {
      #   #     name = system;
      #   #     value = {
      #   #       # nixos = self.nixosConfigurations.phil.config.system.build.toplevel;
      #   #       # server = self.homeConfigurations.server.activationPackage;
      #   #     };
      #   #   })
      #   #   self.lib.platforms.linux)
      # );

    };
}
