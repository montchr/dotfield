{
  description = "Dotfield";

  inputs = {
    stable.url = "github:nixos/nixpkgs/release-21.11";
    latest.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    digga.url = "github:divnix/digga";
    digga.inputs.nixpkgs.follows = "stable";
    digga.inputs.nixlib.follows = "stable";
    digga.inputs.home-manager.follows = "home-manager";

    utils.url = "github:gytis-ivaskevicius/flake-utils-plus";
    nur.url = "github:nix-community/NUR";

    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "stable";
    agenix-cli.url = "github:montchr/agenix-cli/develop";
    # agenix-cli.inputs.nixpkgs.follows = "stable";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "stable";

    darwin.url = "github:montchr/nix-darwin/trunk";
    darwin.inputs.nixpkgs.follows = "stable";

    emacs.url = "github:montchr/emacs/develop";
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    rnix-lsp.url = "github:nix-community/rnix-lsp";
    rnix-lsp.inputs.nixpkgs.follows = "stable";

    nvfetcher.url = "github:berberman/nvfetcher";
    nvfetcher.inputs.nixpkgs.follows = "stable";

    nix-colors.url = "github:montchr/nix-colors";

    firefox-lepton = {
      url = "github:black7375/Firefox-UI-Fix";
      flake = false;
    };

    base16-kitty = {
      url = "github:kdrag0n/base16-kitty";
      flake = false;
    };

    nixpkgs.follows = "stable";
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
    , utils
    , stable
    , latest
    , nur
    , nvfetcher
    , ...
    } @ inputs:
    let
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
          ];
      };

      mkNixosHost = name: extraSuites: {
        ${name} = {
          system = "x86_64-linux";
          modules = suites.base ++ extraSuites ++ [
            hostConfigs.${name}
            home-manager.nixosModules.home-manager
            agenix.nixosModules.age
          ];
        };
      };

      mkDarwinHost = name: extraSuites: {
        ${name} = {
          system = "x86_64-darwin";
          output = "darwinConfigurations";
          builder = darwin.lib.darwinSystem;
          modules = suites.base ++ extraSuites ++ [
            hostConfigs.${name}
            home-manager.darwinModules.home-manager
          ];
        };
      };

      mkHosts = hosts: (builtins.foldl' (a: b: a // b) { } hosts);

    in

    utils.lib.mkFlake {
      inherit self inputs;

      channelsConfig = {
        allowUnfree = true;
      };

      channels = {
        stable = { };
        latest = { };
      };

      lib = import ./lib { lib = digga.lib // latest.lib; };

      sharedOverlays = [
        (import ./pkgs/default.nix)
        (import ./overlays/yabai.nix)
        agenix.overlay
        emacs.overlay
        nur.overlay
        nvfetcher.overlay
        (final: prev: {
          __dontExport = true;
          lib = prev.lib.extend (lfinal: lprev: {
            our = self.lib;
          });
        })
      ];

      hostDefaults = {
        channelName = "stable";
        extraArgs = { inherit utils inputs; };
        specialArgs = { inherit suites systemProfiles userProfiles; };
        modules = [
          ./modules/dotfield.nix
          ./users/modules/user-settings
          ./users/primary-user
          nix-colors.homeManagerModule
        ] ++ (builtins.attrValues (digga.lib.flattenTree
          (digga.lib.rakeLeaves ./users/modules)));
      };

      hosts = with suites;
        mkHosts [
          (mkDarwinHost "HodgePodge" (darwin-gui ++ personal ++ developer ++ home))
          (mkDarwinHost "alleymon" (darwin-gui ++ work ++ personal ++ home))

          # CI runner hosts.
          (mkDarwinHost "ci-darwin" (darwin-minimal ++ developer))
          (mkNixosHost "ci-ubuntu" (linux-minimal ++ developer))
        ];

      # Shortcuts
      HodgePodge = self.darwinConfigurations.HodgePodge.system;
      alleymon = self.darwinConfigurations.alleymon.system;

    };
}
