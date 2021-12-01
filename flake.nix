{
  description = "Dotfield";

  inputs = {
    stable.url = "github:nixos/nixpkgs/release-21.05";
    latest.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    digga.url = "github:divnix/digga";
    digga.inputs.nixpkgs.follows = "latest";
    digga.inputs.nixlib.follows = "latest";
    digga.inputs.home-manager.follows = "home-manager";

    utils.url = "github:gytis-ivaskevicius/flake-utils-plus";
    nur.url = "github:nix-community/NUR";

    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "latest";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "latest";

    darwin.url = "github:montchr/nix-darwin/trunk";
    darwin.inputs.nixpkgs.follows = "latest";

    emacs.url = "github:montchr/emacs/develop";
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    rnix-lsp.url = "github:nix-community/rnix-lsp";
    rnix-lsp.inputs.nixpkgs.follows = "nixpkgs";

    nvfetcher.url = "github:berberman/nvfetcher";
    nvfetcher.inputs.nixpkgs.follows = "latest";
    nvfetcher.inputs.flake-utils.follows = "digga/flake-utils-plus/flake-utils";

    nix-colors.url = "github:montchr/nix-colors";

    firefox-lepton = {
      url = "github:black7375/Firefox-UI-Fix";
      flake = false;
    };

    base16-kitty = {
      url = "github:kdrag0n/base16-kitty";
      flake = false;
    };

    nixpkgs.follows = "latest";
    nixlib.follows = "digga/nixlib";
    blank.follows = "digga/blank";
    utils.follows = "digga/flake-utils-plus";
  };

  outputs =
    { self
    , agenix
    , darwin
    , digga
    , emacs-overlay
    , home-manager
    , nix-colors
    , utils
    , stable
    , latest
    , nixlib
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
          systemProfiles.networking.common
          userProfiles.bash
          userProfiles.bat
          userProfiles.git
          userProfiles.zsh
        ];
        developer = suites.base ++ [
          systemProfiles.languages.nodejs
          userProfiles.direnv
          userProfiles.emacs
          userProfiles.languages.nodejs
        ];
        darwin-minimal = suites.base ++ [
          systemProfiles.darwin.common
        ];
        darwin = suites.darwin-minimal ++ suites.gui ++ [
          systemProfiles.darwin.system-defaults
          userProfiles.darwin.gui
          userProfiles.darwin.keyboard
        ];
        gui = [
          systemProfiles.fonts
          userProfiles.browsers.firefox
          userProfiles.kitty
        ];
        secrets = [
          systemProfiles.secrets
          userProfiles.secrets
        ];
        personal = suites.secrets ++ [
          systemProfiles.security.yubikey
          userProfiles.gnupg
          userProfiles.mail
          userProfiles.pass
          userProfiles.security.yubikey
          userProfiles.ssh
        ];
        work = suites.developer ++
          suites.darwin ++
          suites.personal ++
          [
            systemProfiles.languages.php
            systemProfiles.languages.ruby # for vagrant
          ];
      };

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
        emacs-overlay.overlay
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
        channelName = "latest";
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

      hosts = with suites; let
        mkNixosHost = name: extraSuites: { minimal ? true }: {
          system = "x86_64-linux";
          modules = suites.base ++ extraSuites ++ [
            hostConfigs.${name}
            home-manager.nixosModules.home-manager
            agenix.nixosModules.age
          ];
        };
        mkDarwinHost = name: extraSuites: { minimal ? true }: {
          system = "x86_64-darwin";
          output = "darwinConfigurations";
          builder = darwin.lib.darwinSystem;
          modules = (if minimal then darwin-minimal else darwin) ++
            extraSuites ++
            [
              hostConfigs.${name}
              home-manager.darwinModules.home-manager
            ];
        };
      in
      {
        HodgePodge = (mkDarwinHost "HodgePodge" (personal ++ developer) { });
        alleymon = (mkDarwinHost "alleymon" (work ++ secrets) { });
        ghaDarwin = (mkDarwinHost "ghaDarwin" [ ] { minimal = true; });
        ghaUbuntu = (mkNixosHost "ghaUbuntu" [ ] { minimal = true; });
      };

      # Shortcuts
      HodgePodge = self.darwinConfigurations.HodgePodge.system;
      alleymon = self.darwinConfigurations.alleymon.system;
      ghaDarwin = self.darwinConfigurations.ghaDarwin.system;
      ghaUbuntu = self.nixosConfigurations.ghaUbuntu.system;

    };
}
