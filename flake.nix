{
  description = "Dotfield";

  inputs = {
    # Package sets
    nixos-stable.url = "github:NixOS/nixpkgs/release-21.11";
    nixpkgs-trunk.url = github:NixOS/nixpkgs/master;
    nixpkgs-darwin-stable.url = github:NixOS/nixpkgs/nixpkgs-21.11-darwin;
    nixpkgs-unstable.url = github:NixOS/nixpkgs/nixpkgs-unstable;

    # Environment/system management.
    darwin.url = github:montchr/nix-darwin/trunk;
    darwin.inputs.nixpkgs.follows = "nixpkgs-unstable";
    home-manager.url = github:nix-community/home-manager;
    home-manager.inputs.nixpkgs.follows = "nixpkgs-unstable";

    # Flake utilities.
    digga.url = github:divnix/digga;
    digga.inputs.nixpkgs.follows = "nixos-stable";
    digga.inputs.nixlib.follows = "nixos-stable";
    digga.inputs.home-manager.follows = "home-manager";
    utils.url = github:gytis-ivaskevicius/flake-utils-plus;
    flake-compat = { url = github:edolstra/flake-compat; flake = false; };

    # Sources management.
    nur.url = github:nix-community/NUR;
    nvfetcher.url = github:berberman/nvfetcher;

    # Secrets management.
    agenix.url = github:ryantm/agenix;
    agenix.inputs.nixpkgs.follows = "nixos-stable";
    agenix-cli.url = github:montchr/agenix-cli/develop;

    # Development tools.
    emacs.url = github:montchr/emacs/develop;
    emacs-overlay.url = github:nix-community/emacs-overlay;
    rnix-lsp.url = github:nix-community/rnix-lsp;

    # Other sources.
    # prefmanager.url = github:malob/prefmanager;
    # prefmanager.inputs.nixpkgs.follows = "nixpkgs-unstable";
    base16-kitty = { url = github:kdrag0n/base16-kitty; flake = false; };
    firefox-lepton = { url = github:black7375/Firefox-UI-Fix; flake = false; };
    nix-colors.url = github:montchr/nix-colors;

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

      hostConfigs = digga.lib.rakeLeaves ./hosts;
      systemProfiles = digga.lib.rakeLeaves ./profiles;
      userProfiles = digga.lib.rakeLeaves ./users/profiles;
      suites = rec {
        base = [
          systemProfiles.core
          userProfiles.bash
          userProfiles.bat
          userProfiles.git
          userProfiles.zsh
        ];
        networking = [
          systemProfiles.networking.common
        ];
        linux-minimal = suites.base ++ [
          systemProfiles.linux
        ];
        nixos = suites.base ++ [
          systemProfiles.linux
          systemProfiles.nixos
        ];
        darwin-minimal = suites.base ++ [
          systemProfiles.darwin.common
        ];
        darwin-gui = suites.base ++ suites.gui ++ [
          systemProfiles.darwin.common
          systemProfiles.darwin.system-defaults
          userProfiles.darwin.gui
          userProfiles.darwin.keyboard
        ];
        developer = suites.base ++ [
          systemProfiles.languages.nodejs
          userProfiles.direnv
          userProfiles.emacs
          userProfiles.languages.nodejs
        ];
        gui = [
          systemProfiles.fonts
          userProfiles.browsers.firefox
          userProfiles.espanso
          userProfiles.kitty
        ];
        personal = [
          systemProfiles.security.yubikey
          systemProfiles.secrets
          userProfiles.gnupg
          userProfiles.mail
          userProfiles.pass
          userProfiles.security.yubikey
          userProfiles.secrets
          userProfiles.ssh
        ];
        home = [
          userProfiles.home
        ];
        work = suites.developer ++
          suites.personal ++
          [
            systemProfiles.languages.php
            systemProfiles.languages.ruby # for vagrant
            systemProfiles.virtualbox
          ];
      };

      mkHosts = hosts: (builtins.foldl' (a: b: a // b) { } hosts);

      systemImportables = rec {
        profiles = (digga.lib.rakeLeaves ./profiles) // {
          users = (digga.lib.rakeLeaves ./users);
        };
        suites = with profiles; { };
      };

      userImportables = rec {
        profiles = (digga.lib.rakeLeaves ./users/profiles);
        suites = with profiles; rec { };
      };

      mkNixosHost = name:
        { system ? "x86_64-linux"
        , channelName ? "nixos-stable"
        , extraSuites ? [ ]
        }: {
          ${name} = {
            inherit system channelName;
            specialArgs = { inherit (systemImportables) profiles suites; };
            modules = suites.base ++ extraSuites ++ [
              hostConfigs.${name}
              home-manager.nixosModules.home-manager
              agenix.nixosModules.age
            ];
          };
        };

      mkDarwinHost = name:
        { system ? "x86_64-darwin"
        , channelName ? "nixpkgs-darwin-stable"
        , extraSuites ? [ ]
        }: {
          ${name} = {
            inherit system channelName;
            output = "darwinConfigurations";
            builder = darwin.lib.darwinSystem;
            specialArgs = { inherit (systemImportables) profiles suites; };
            modules = suites.base ++ extraSuites ++ [
              hostConfigs.${name}
              home-manager.darwinModules.home-manager
            ];
          };
        };

      # https://github.com/kclejeune/system/blob/71c65173e7eba8765a3962df5b52c2f2c25a8fac/flake.nix#L89-L109
      # generate a home-manager configuration usable on any unix system
      # with overlays and any extraModules applied
      mkPortableHomeConfiguration =
        { username
        , system
        , extraExtraModules ? [ ]
        }:
        home-manager.lib.homeManagerConfiguration rec {
          inherit system username;
          inherit (hmCommonConfig) extraSpecialArgs;

          homeDirectory = self.lib.mkHomePath username;
          extraModules = hmCommonConfig.extraModules // extraExtraModules;

          configuration = {
            imports = [ configuration ];
          } // (self.lib.optionalAttrs (self.lib.our.isGenericLinux system)
            { targets.genericLinux.enable = true; });
        };

      homeConfigurationsPortable = { pkgs, system }:
        builtins.mapAttrs
          (n: v: mkPortableHomeConfiguration {
            inherit pkgs system;
            username = n;
            configuration = v;
          })
          hmUsers;

      hmUsers = {
        montchr = { suites, ... }: { imports = [ suites.base ]; };
      };

      hmCommonConfig = {
        extraModules = [ self.hmModules ];
        # extraModules = [ (digga.lib.importExportableModules ./users/modules) ];
        extraSpecialArgs = userImportables // {
          inherit self inputs;
          # inherit (self) lib;
        };
      };

      hmDefaults = { ... }: {
        home-manager = {
          # Always use the system nixpkgs from the host's channel
          useGlobalPkgs = true;
          # And use the possible future default (see manual)
          useUserPackages = self.lib.mkDefault true;

          extraSpecialArgs = userImportables // { inherit self inputs; };
          sharedModules = hmCommonConfig.extraModules;
        };
      };

    in

    utils.lib.mkFlake {
      inherit self inputs;
      inherit homeConfigurationsPortable;

      hmModules = (digga.lib.importExportableModules ./users/modules);
      # homeModules = utils.lib.exportModules config.home.exportedModules;

      channelsConfig.allowUnfree = true;

      lib = import ./lib { lib = digga.lib // nixpkgs-unstable.lib; };

      channels = {
        nixos-stable = { };
        nixpkgs-trunk = { };
        nixpkgs-darwin-stable = {
          overlaysBuilder = (channels: [
            (import ./pkgs/darwin)
            (final: prev: {
              inherit (channels.nixpkgs-unstable)
                direnv
                nix-direnv
                # yabai
                ;
            })
            emacs.overlay
          ]);
        };
        nixpkgs-unstable = {
          overlaysBuilder = (channels: [
            (import ./overlays/darwin/yabai.nix)
          ]);
        };
      };

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
        specialArgs = { inherit suites systemProfiles userProfiles; };
        modules = [
          ./users/primary-user
          nix-colors.homeManagerModule
          hmDefaults
          ({ ... }: {
            _module.args = {
              inherit hmUsers;
            };
          })
        ] ++ (builtins.attrValues (digga.lib.flattenTree
          (digga.lib.rakeLeaves ./modules)));
      };

      hosts = with suites;
        mkHosts [
          (mkDarwinHost "HodgePodge" { extraSuites = (darwin-gui ++ personal ++ developer ++ home); })
          (mkDarwinHost "alleymon" { extraSuites = (darwin-gui ++ work ++ personal ++ home); })

          # CI runner hosts.
          (mkDarwinHost "ci-darwin" { extraSuites = (darwin-minimal ++ developer); })
          (mkNixosHost "ci-ubuntu" { extraSuites = (linux-minimal ++ developer); })
        ];

      homeConfigurations = digga.lib.mkHomeConfigurations (self.nixosConfigurations // self.darwinConfigurations);

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
