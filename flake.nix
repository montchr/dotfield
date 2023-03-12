{
  description = "Dotfield";

  inputs = {
    nixpkgs.follows = "nixos-unstable";
    nixos-stable.url = "github:NixOS/nixpkgs/nixos-22.11";
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-trunk.url = "github:NixOS/nixpkgs/master";

    ##: iosevka-bin versions
    #
    # Reduce churn by tracking various identically-named sources while waiting
    # for upstream versions to become available.
    nixpkgs-update-iosevka-bin.follows = "nixos-unstable";
    # nixpkgs-update-iosevka-bin.url = "github:montchr/nixpkgs/update-iosevka-bin";
    # nixpkgs-update-iosevka-bin.url = "github:r-ryantm/nixpkgs?ref=auto-update/iosevka-bin";

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
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager-pr-2964 = {
      url = "github:montchr/home-manager/gpg-agent-darwin";
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
      url = "github:divnix/std";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ##: forks
    agenix.url = "github:montchr/agenix/darwin-support";

    ##: sources
    iosevka-xtal.url = "github:montchr/iosevka-xtal";

    ##: work
    klein-infra = {
      url = "github:kleinweb/infra";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.std.follows = "std";
      inputs.dmerge.follows = "std/dmerge";
    };

    ##: universal
    deadnix.url = "github:astro/deadnix";
    devshell.url = "github:numtide/devshell";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils.url = "github:numtide/flake-utils";
    gitignore.url = "github:hercules-ci/gitignore.nix";
    nix-std.url = "github:chessai/nix-std";
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

    ##: firefox
    firefox-addons.url = "github:seadome/firefox-addons";

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
    flake-parts,
    nixpkgs,
    digga,
    ...
  } @ inputs: let
    inherit (digga.lib) flattenTree rakeLeaves;
    peers = import ./ops/metadata/peers.nix;
    # FIXME: move to guardian
    primaryUser.authorizedKeys = import ./secrets/authorized-keys.nix;
  in (flake-parts.lib.mkFlake {inherit inputs;} {
    systems = ["aarch64-darwin" "aarch64-linux" "x86_64-darwin" "x86_64-linux"];
    std.grow.cellsFrom = ./cells;
    std.grow.cellBlocks = with inputs.std.blockTypes; [
      (data "constants")
      (devshells "devshells")
      (functions "dev")
      (functions "devshellProfiles")
      (functions "functions")
      (installables "packages")
      (nixago "cfg")
    ];
    std.harvest = {
      devShells = [
        ["dotfield" "devshells"]
        ["_automation" "devshells"]
        ["secrets" "devshells"]
      ];
    };
    # FIXME: this is required, but shouldn't be -- needs upstream fix
    # https://github.com/divnix/std/issues/235
    std.grow.nixpkgsConfig = {allowUnfree = true;};
    imports = [
      inputs.std.flakeModule

      {_module.args = {inherit inputs peers primaryUser;};}

      ./flake-modules/homeConfigurations.nix
      ./flake-modules/sharedModules.nix
      ./flake-modules/sharedProfiles.nix

      ./lib
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
      self',
      ...
    }: {
      _module.args = {
        inherit (self') cells;
        inherit primaryUser;
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };
      };
      formatter = inputs'.nixpkgs.legacyPackages.alejandra;
    };
    flake = {
      # shared importables :: may be used within system configurations for any
      # supported operating system (e.g. nixos, nix-darwin).
      sharedModules = flattenTree (rakeLeaves ./modules);
      sharedProfiles = rakeLeaves ./profiles;
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
