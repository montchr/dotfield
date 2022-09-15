{
  description = "Dotfield";

  # Enable as needed for bootstrapping. Otherwise these entries cause warnings on every rebuild.
  # nixConfig.extra-experimental-features = "nix-command flakes";
  # nixConfig.extra-substituters = "https://dotfield.cachix.org https://nix-community.cachix.org";
  # nixConfig.extra-trusted-public-keys = "dotfield.cachix.org-1:b5H/ucY/9PDARWG9uWA87ZKWUBU+hnfF30amwiXiaNk= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=";

  inputs = {
    nixos-stable.url = "github:NixOS/nixpkgs/nixos-22.05";
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-trunk.url = "github:NixOS/nixpkgs/master";
    nixpkgs-darwin-stable.url = "github:NixOS/nixpkgs/nixpkgs-22.05-darwin";
    nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland";
    nur.url = "github:nix-community/NUR";

    darwin.url = "github:LnL7/nix-darwin";
    digga = {
      url = "github:divnix/digga/home-manager-22.11";
      inputs.home-manager.follows = "home-manager";
    };
    home-manager.url = "github:nix-community/home-manager";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils.url = "github:numtide/flake-utils";

    agenix.url = "github:montchr/agenix/darwin-support";
    deploy-rs.url = "github:serokell/deploy-rs";
    devshell.url = "github:numtide/devshell";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    gitignore.url = "github:hercules-ci/gitignore.nix";
    mozilla-addons-to-nix.url = "sourcehut:~rycee/mozilla-addons-to-nix";
    nix-dram = {
      url = "github:dramforever/nix-dram";
      inputs.flake-utils.follows = "flake-utils";
    };
    nix-colors.url = "github:Misterio77/nix-colors";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nixos-generators.url = "github:nix-community/nixos-generators";
    nixago.url = "github:nix-community/nixago";
    prefmanager.url = "github:malob/prefmanager";
    sops-nix.url = "github:Mic92/sops-nix";
    rnix-lsp.url = "github:nix-community/rnix-lsp";

    # TODO: use nix-colors or something
    base16-kitty = {
      url = "github:kdrag0n/base16-kitty";
      flake = false;
    };

    digga.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    flake-parts.inputs.nixpkgs.follows = "nixpkgs";
    nix-dram.inputs.nixpkgs.follows = "nixpkgs";
    nixago.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs-wayland.inputs.nixpkgs.follows = "nixos-unstable";

    nixpkgs.follows = "nixos-unstable";
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
    nur,
    nix-dram,
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
      eso = import ./lib {
        inherit peers;
        flake = self;
        lib = lfinal;
      };
    });

    exoOverlays = [
      nixpkgs-wayland.overlay
      emacs-overlay.overlay
      nur.overlay
      nix-dram.overlay
    ];

    esoOverlays = [
      (final: prev: {inherit lib;})
      self.overlays.packages
      self.overlays.overrides
    ];

    # shared importables :: may be used within system configurations for any
    # supported operating system (e.g. nixos, nix-darwin).
    peers = import ./ops/metadata/peers.nix;
    sharedModules = flattenTree (rakeLeaves ./modules);
    sharedProfiles = rakeLeaves ./profiles;
  in (flake-parts.lib.mkFlake {inherit self;} {
    systems = supportedSystems;
    imports = [
      {
        _module.args.peers = peers;
      }

      ./flake-modules/sharedModules.nix
      ./flake-modules/sharedProfiles.nix

      ./devShells
      ./overlays
      ./packages
      ./darwin/packages

      ./darwin/configurations.nix
      ./home/configurations.nix
      ./nixos/configurations.nix
    ];
    perSystem = {system, ...}: {
      _module.args.pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = exoOverlays ++ esoOverlays;
      };
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
