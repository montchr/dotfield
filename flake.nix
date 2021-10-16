{
  description = "Dotfield";

  inputs = {
    stable.url = "github:nixos/nixpkgs/release-21.05";
    latest.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs.follows = "latest";

    utils.url = "github:gytis-ivaskevicius/flake-utils-plus";
    nur.url = "github:nix-community/NUR";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "latest";

    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "latest";

    emacs.url = "github:cmacrae/emacs?rev=8bbbdae607d3f03a8e6c488b310d9443f6ff11bf";
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    rnix-lsp.url = "github:nix-community/rnix-lsp";
    rnix-lsp.inputs.nixpkgs.follows = "nixpkgs";

    firefox-lepton = {
      url = "github:black7375/Firefox-UI-Fix";
      flake = false;
    };

    base16-kitty = {
      url = "github:kdrag0n/base16-kitty";
      flake = false;
    };
  };

  outputs =
    { self
    , darwin
    , emacs
    , emacs-overlay
    , home-manager
    , utils
    , latest
    , nur
    , ...
    } @ inputs:
    utils.lib.mkFlake {
      inherit self inputs;

      channelsConfig = {
        allowUnfree = true;
      };

      sharedOverlays = [
        (import ./overlays/yabai.nix)
        emacs.overlay
        # emacs-overlay.overlay
        nur.overlay
        (final: prev: {
          nix-direnv = (prev.nix-direnv.override { enableFlakes = true; });
          pragmatapro = (prev.callPackage ./pkgs/pragmatapro.nix { });
        })
      ];

      hostDefaults = {
        channelName = "latest";
        extraArgs = { inherit utils inputs; };
        modules = [
          ./modules
          ./profiles/core
          ./suites/personal
          ./users
        ];
      };

      hosts =
        let
          darwinHostDefaults = {
            system = "x86_64-darwin";
            output = "darwinConfigurations";
            builder = darwin.lib.darwinSystem;
            modules = [
              home-manager.darwinModules.home-manager
            ];
          };
        in
        {
          HodgePodge = utils.lib.mergeAny darwinHostDefaults {
            modules = [ ./hosts/hodgepodge.nix ];
          };
          alleymon = utils.lib.mergeAny darwinHostDefaults {
            modules = [ ./hosts/alleymon.nix ];
          };
        };

      # Shortcuts
      HodgePodge = self.darwinConfigurations.HodgePodge.system;
      alleymon = self.darwinConfigurations.alleymon.system;

    };
}
