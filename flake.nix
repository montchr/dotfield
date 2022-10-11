{
  description = "Dotfield";

  # Enable as needed for bootstrapping. Otherwise these entries cause warnings on every rebuild.
  # nixConfig.extra-experimental-features = "nix-command flakes";
  # nixConfig.extra-substituters = "https://dotfield.cachix.org https://nix-community.cachix.org";
  # nixConfig.extra-trusted-public-keys = "dotfield.cachix.org-1:b5H/ucY/9PDARWG9uWA87ZKWUBU+hnfF30amwiXiaNk= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=";

  inputs = {
    nixpkgs.follows = "nixos-unstable";
    nixos-stable.url = "github:NixOS/nixpkgs/nixos-22.05";
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-trunk.url = "github:NixOS/nixpkgs/master";

    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    digga = {
      url = "github:divnix/digga/home-manager-22.11";
      inputs.home-manager.follows = "home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
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

    agenix.url = "github:montchr/agenix/darwin-support";
    deploy-rs.url = "github:serokell/deploy-rs";
    devshell.url = "github:numtide/devshell";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    gitignore.url = "github:hercules-ci/gitignore.nix";
    mozilla-addons-to-nix.url = "sourcehut:~rycee/mozilla-addons-to-nix";
    nix-std.url = "github:chessai/nix-std";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nixos-generators.url = "github:nix-community/nixos-generators";
    prefmanager.url = "github:malob/prefmanager";
    sops-nix.url = "github:Mic92/sops-nix";
    nil-lsp.url = "github:oxalica/nil";
    rnix-lsp.url = "github:nix-community/rnix-lsp";

    # TODO: use nix-colors or something
    base16-kitty = {
      url = "github:kdrag0n/base16-kitty";
      flake = false;
    };
  };

  outputs = {
    self,
    nixpkgs,
    nixos-unstable,
    flake-parts,
    digga,
    flake-utils,
    nixpkgs-wayland,
    emacs-overlay,
    nix-dram,
    nix-std,
    ...
  }: let
    inherit (digga.lib) flattenTree rakeLeaves;

    supportedSystems = with flake-utils.lib.system; [
      x86_64-linux
      aarch64-linux
      x86_64-darwin
      aarch64-darwin
    ];

    lib = nixos-unstable.lib.extend (lfinal: lprev: {
      std = nix-std;
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
      (final: prev: {inherit lib;})
      self.overlays.sources
      self.overlays.packages
      self.overlays.iosevka
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
  in (flake-parts.lib.mkFlake {inherit self;} {
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

      ./devShells
      ./overlays
      ./packages

      ./nixos/configurations.nix
      ./nixos/checks.nix

      ./home/configurations.nix

      ./darwin/configurations.nix
      ./darwin/packages
    ];
    perSystem = {system, ...}: let
      # FIXME: while this fixes builds on `aarch64-darwin`, checks fail:
      # => a 'x86_64-linux' with features {} is required to build...
      nixpkgs' = (import nixpkgs {inherit system;}).applyPatches {
        name = "nixpkgs-patched-for-aarch64-darwin";
        src = nixpkgs;
        patches = [
          # https://github.com/NixOS/nixpkgs/pull/193589 <- 2022-10-10: waiting for review since 2022-10-01
          ./packages/patches/nixos-nixpkgs-193589.patch
          # https://github.com/NixOS/nixpkgs/pull/194308 <- 2022-10-10: merged into `staging` a week ago
          ./packages/patches/nixos-nixpkgs-194308.patch
        ];
      };
      pkgs = import nixpkgs' {
        inherit system;
        config.allowUnfree = true;
        overlays = exoOverlays ++ esoOverlays;
      };
    in {
      _module.args = {inherit pkgs primaryUser;};
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
}
