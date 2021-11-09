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

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "latest";

    darwin.url = "github:lnl7/nix-darwin/master";
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
    , darwin
    , digga
    , emacs
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
          userProfiles.direnv
          userProfiles.emacs
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
          userProfiles.kitty
        ];
        personal = [
          userProfiles.gnupg
          userProfiles.mail
          userProfiles.pass
        ];
        vagrant = [
          systemProfiles.languages.ruby
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
        channelName = "latest";
        extraArgs = { inherit utils inputs; };
        specialArgs = { inherit suites systemProfiles userProfiles; };

        modules = [
          ./modules
          ./modules/dotfield.nix
          ./users/primary-user
          nix-colors.homeManagerModule
        ] ++ (builtins.attrValues (digga.lib.flattenTree
          (digga.lib.rakeLeaves ./users/modules)));
      };

      hosts =
        let
          mkDarwinHost = name:
            { minimal ? false
            , extraSuites ? [ ]
            }: {
              system = "x86_64-darwin";
              output = "darwinConfigurations";
              builder = darwin.lib.darwinSystem;
              modules = (if minimal then suites.darwin-minimal else suites.darwin) ++
                extraSuites ++
                [
                  hostConfigs.${name}
                  home-manager.darwinModules.home-manager
                ];
            };
        in
        {
          HodgePodge = (mkDarwinHost "HodgePodge" {
            extraSuites = suites.personal ++ suites.developer;
          });
          alleymon = (mkDarwinHost "alleymon" {
            extraSuites = suites.personal ++ suites.developer ++ suites.vagrant;
          });
          ghaDarwin = (mkDarwinHost "ghaDarwin" { minimal = true; });
          ghaUbuntu = {
            modules = suites.base ++ [
              home-manager.nixosModules.home-manager
            ];
          };
        };

      # Shortcuts
      HodgePodge = self.darwinConfigurations.HodgePodge.system;
      alleymon = self.darwinConfigurations.alleymon.system;
      ghaDarwin = self.darwinConfigurations.ghaDarwin.system;
      ghaUbuntu = self.nixosConfigurations.ghaUbuntu.system;

    };
}
