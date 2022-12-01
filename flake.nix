{
  description = "Dotfield";

  inputs = {
    nixpkgs.follows = "nixos-unstable";
    nixos-stable.url = "github:NixOS/nixpkgs/nixos-22.05";
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-trunk.url = "github:NixOS/nixpkgs/master";
    nixpkgs-fork-update-iosevka.url = "github:montchr/nixpkgs/iosevka-v16.4.0";
    nixpkgs-fork-add-lint-staged.url = "github:montchr/nixpkgs/add-lint-staged";

    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    digga = {
      url = "github:divnix/digga/home-manager-22.11";
      inputs.home-manager.follows = "home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager-pr-2964 = {
      # url = "github:montchr/home-manager/gpg-agent-darwin";
      # Includes changes from https://github.com/nix-community/home-manager/pull/3183
      url = "github:montchr/home-manager/gpg-agent-darwin-with-pinentry-mac";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    macos-builder = {
      url = "github:Gabriella439/macos-builder";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    microvm = {
      url = "github:astro/microvm.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-colors = {
      url = "github:Misterio77/nix-colors";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    nix-dram = {
      url = "github:dramforever/nix-dram";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixago = {
      url = "github:nix-community/nixago";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs-wayland = {
      url = "github:nix-community/nixpkgs-wayland";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    std = {
      # url = "github:divnix/std";
      url = "github:divnix/std/fix-editorconfig-hack";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ##: forks
    agenix.url = "github:montchr/agenix/darwin-support";

    ##: sources
    iosevka-xtal.url = "github:montchr/iosevka-xtal";

    ##: universal
    deadnix.url = "github:astro/deadnix";
    devshell.url = "github:numtide/devshell";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    flake-parts.url = "github:hercules-ci/flake-parts";
    # FIXME: remove flake-utils (provided by nixlib)
    flake-utils.url = "github:numtide/flake-utils";
    gitignore.url = "github:hercules-ci/gitignore.nix";
    stdlib.url = "github:chessai/nix-std";
    sops-nix.url = "github:Mic92/sops-nix";
    nil-lsp.url = "github:oxalica/nil";
    rnix-lsp.url = "github:nix-community/rnix-lsp";

    ##: darwin
    prefmanager.url = "github:malob/prefmanager";

    ##: linux
    deploy-rs.url = "github:serokell/deploy-rs";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nixos-generators.url = "github:nix-community/nixos-generators";

    ##: emacs
    chemacs.url = "github:plexus/chemacs2";
    chemacs.flake = false;

    ##: zsh plugins
    zsh-autopair.url = "github:hlissner/zsh-autopair";
    zsh-autopair.flake = false;
    zsh-completions.url = "github:zsh-users/zsh-completions";
    zsh-completions.flake = false;
    zsh-fast-syntax-highlighting.url = "github:zdharma-continuum/fast-syntax-highlighting";
    zsh-fast-syntax-highlighting.flake = false;
  };

  outputs = {
    self,
    std,
    nixpkgs,
    nixos-unstable,
    flake-parts,
    digga,
    flake-utils,
    nixpkgs-wayland,
    emacs-overlay,
    nix-dram,
    stdlib,
    ...
  } @ inputs: let
    inherit (digga.lib) flattenTree rakeLeaves;
    inherit (std) blockTypes growOn harvest;

    supportedSystems = with flake-utils.lib.system; [
      x86_64-linux
      aarch64-linux
      x86_64-darwin
      aarch64-darwin
    ];

    lib = nixos-unstable.lib.extend (lfinal: _lprev: {
      std = stdlib;
      eso = import ./lib {
        inherit (self) inputs;
        inherit peers;
        lib = lfinal;
      };
    });

    exoOverlays = [
      nixpkgs-wayland.overlay
      emacs-overlay.overlay
      nix-dram.overlay
      self.overlays.externalPackages
    ];

    esoOverlays = [
      (_final: _prev: {inherit lib;})
      self.overlays.sources
      self.overlays.packages
      self.overlays.firefox-addons
      self.overlays.overrides
    ];

    # FIXME: move to guardian
    primaryUser.authorizedKeys = import ./secrets/authorized-keys.nix;

    # shared importables :: may be used within system configurations for any
    # supported operating system (e.g. nixos, nix-darwin).
    peers = import ./ops/metadata/peers.nix;
    sharedModules = flattenTree (rakeLeaves ./modules);
    sharedProfiles = rakeLeaves ./profiles;
  in
    growOn {
      inherit inputs;
      cellsFrom = ./cells;
      cellBlocks = [
        ##: lib
        (blockTypes.functions "dev")
        (blockTypes.functions "functions")
        (blockTypes.nixago "nixago")
        (blockTypes.installables "packages")

        ##: hosts
        (blockTypes.data "modules")
        (blockTypes.data "profiles")
        (blockTypes.data "hosts")
        (blockTypes.data "compat")

        ##: automation
        (blockTypes.data "constants")
        (blockTypes.data "devshellProfiles")
        (blockTypes.devshells "devshells")
        (blockTypes.nixago "nixago")
        (blockTypes.installables "packages")
      ];
    }
    {
      devShells = harvest self [["dotfield" "devshells"] ["_automation" "devshells"]];
    }
    (flake-parts.lib.mkFlake {inherit self;} {
      systems = supportedSystems;
      imports = [
        {
          _module.args = {
            inherit peers primaryUser;
          };
        }

        ./flake-modules/homeConfigurations.nix
        ./flake-modules/sharedModules.nix
        ./flake-modules/sharedProfiles.nix

        ./overlays
        ./packages

        ./nixos/configurations.nix
        ./nixos/checks.nix

        ./home/configurations.nix

        ./darwin/configurations.nix
        ./darwin/packages
      ];
      perSystem = {
        system,
        inputs',
        ...
      }: let
        pkgsets = {
          default = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
            overlays = exoOverlays ++ esoOverlays;
          };
          stable = inputs'.nixos-stable.legacyPackages;
          unstable = inputs'.nixos-unstable.legacyPackages;
          trunk = inputs'.nixpkgs-trunk.legacyPackages;
          forked-with-lint-staged = inputs'.nixpkgs-fork-add-lint-staged.legacyPackages;
        };
        pkgs = pkgsets.default;
      in {
        _module.args = {inherit pkgs primaryUser pkgsets;};
        formatter = pkgs.alejandra;
      };
      flake = {
        inherit
          sharedModules
          sharedProfiles
          ;

        lib = lib.eso;

        # deploy.nodes = digga.lib.mkDeployNodes self.nixosConfigurations {
        #   tsone = with (peers.hosts.tsone); {
        #     hostname = ipv4.address;
        #     sshUser = "root";
        #     fastConnection = true;
        #     autoRollback = true;
        #     magicRollback = true;
        #   };
        # };
      };
    });

  nixConfig = {
    extra-experimental-features = "nix-command flakes";
    extra-substituters = [
      "https://dotfield.cachix.org"
      "https://iosevka-xtal.cachix.org"
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "dotfield.cachix.org-1:b5H/ucY/9PDARWG9uWA87ZKWUBU+hnfF30amwiXiaNk="
      "iosevka-xtal.cachix.org-1:5d7Is01fs3imwU9w5dom2PcSskJNwtJGbfjRxunuOcw="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
}
