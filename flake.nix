# Thank you to https://github.com/ahmedelgabri/dotfiles/

# As a first step, I will try to symlink my configs as much as possible then
# migrate the configs to Nix
#
# https://nixcloud.io/ for Nix syntax
# https://nix.dev/
# https://nix-community.github.io/awesome-nix/
# https://discourse.nixos.org/t/home-manager-equivalent-of-apt-upgrade/8424/3
# https://www.reddit.com/r/NixOS/comments/jmom4h/new_neofetch_nixos_logo/gayfal2/
# https://www.youtube.com/user/elitespartan117j27/videos?view=0&sort=da&flow=grid
# https://www.youtube.com/playlist?list=PLRGI9KQ3_HP_OFRG6R-p4iFgMSK1t5BHs
# https://www.reddit.com/r/NixOS/comments/k9xwht/best_resources_for_learning_nixos/
# https://www.reddit.com/r/NixOS/comments/k8zobm/nixos_preferred_packages_flow/
# https://www.reddit.com/r/NixOS/comments/j4k2zz/does_anyone_use_flakes_to_manage_their_entire/
#
# Sample repos
# https://github.com/teoljungberg/dotfiles/tree/master/nixpkgs (contains custom hammerspoon & vim )
# https://github.com/jwiegley/nix-config (nice example)
# https://github.com/hardselius/dotfiles (good readme on steps to do for install)
# https://github.com/nuance/dotfiles
#
# With flakes
# https://github.com/hlissner/dotfiles
# https://github.com/mjlbach/nix-dotfiles
# https://github.com/thpham/nix-configs/blob/e46a15f69f/default.nix (nice example of how to build)
# https://github.com/sandhose/nixconf
# https://github.com/cideM/dotfiles (darwin, NixOS, home-manager)
# https://github.com/monadplus/nixconfig
# https://github.com/jamesottaway/dotfiles (Darwin, NixOS, home-manager)
# https://github.com/malob/nixpkgs (Darwin, home-manager, homebrew in nix & PAM, this is a great example)
# https://github.com/kclejeune/system (nice example)
# https://github.com/mrkuz/nixos
#

{
  description = "~ 🍭 ~";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    comma = {
      url = "github:Shopify/comma";
      flake = false;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    rnix-lsp = {
      url = "github:nix-community/rnix-lsp";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # pragmatapro = {
    #   url = "sourcehut:montchr/pragmata-pro";
    #   flake = false;
    # };

    # Extras
    # nixos-hardware.url = "github:nixos/nixos-hardware";
  };

  outputs = { self, ... }@inputs:
    let
      sharedHostsConfig = { config, pkgs, lib, options, ... }: {
        nix = {
          nixPath = [
            "nixpkgs=${inputs.nixpkgs}"
            "darwin=${inputs.darwin}"
            "home-manager=${inputs.home-manager}"
          ];
          package = pkgs.nixFlakes;
          extraOptions = "experimental-features = nix-command flakes";
          binaryCaches = [
            "https://cache.nixos.org"
            "https://nix-community.cachix.org"
            "https://nixpkgs.cachix.org"
          ];
          binaryCachePublicKeys = [
            "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
            "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
            "nixpkgs.cachix.org-1:q91R6hxbwFvDqTSDKwDAV4T5PxqXGxswD8vhONFMeOE="
          ];
          # gc = {
          #   automatic = true;
          #   options = "--delete-older-than 3d";
          # };
        };

        fonts = (lib.mkMerge [
          # [note] Remove this condition when `nix-darwin` aligns with NixOS
          (if (builtins.hasAttr "fontDir" options.fonts) then {
            fontDir.enable = true;
          } else {
            enableFontDir = true;
          })
          # { fonts = with pkgs; [ pragmatapro ]; }
        ]);

        nixpkgs = {
          config = { allowUnfree = true; };
          overlays = [ self.overlay ];
        };

        time.timeZone = config.my.timezone;
      };

    in {
      overlay = (final: prev: {
        # pragmatapro = (prev.callPackage ./dotfield/pkgs/pragmatapro.nix { });
        comma = import inputs.comma { inherit (prev) pkgs; };
      });

      darwinConfigurations = {
        "hodgepodge" = inputs.darwin.lib.darwinSystem {
          inputs = inputs;
          modules = [
            inputs.home-manager.darwinModules.home-manager
            ./dotfield/modules/shared
            sharedHostsConfig
            ./dotfield/hosts/hodgepodge.nix
          ];
        };

        "alleymon" = inputs.darwin.lib.darwinSystem {
          inputs = inputs;
          modules = [
            inputs.home-manager.darwinModules.home-manager
            ./dotfield/modules/shared
            sharedHostsConfig
            ./dotfield/hosts/alleymon.nix
          ];
        };
      };

      # for convenience
      # nix build './#darwinConfigurations.hodgepodge.system'
      # vs
      # nix build './#HodgePodge'
      # Move them to `outputs.packages.<system>.name`
      HodgePodge = self.darwinConfigurations.hodgepodge.system;
      alleymon = self.darwinConfigurations.alleymon.system;

      # [todo] very alpha, needs work
      # nixosConfigurations = {
      #   "nixos" = inputs.nixpkgs.lib.nixosSystem {
      #     system = "x86_64-linux";
      #     specialArgs = { inherit inputs; };
      #     modules = [
      #       inputs.home-manager.nixosModules.home-manager
      #       ./nix/modules/shared
      #       sharedHostsConfig
      #       ./nix/hosts/nixos
      #     ];
      #   };
      # };
    };
}
