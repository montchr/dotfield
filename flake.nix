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
    nixpkgs-darwin-stable.url = "github:NixOS/nixpkgs/nixpkgs-22.05-darwin";
    nixpkgs-wayland = {
      url = "github:nix-community/nixpkgs-wayland";
      # Packages are built against this channel.
      inputs.nixpkgs.follows = "nixos-unstable";
    };
    nur.url = "github:nix-community/NUR";

    darwin.url = "github:LnL7/nix-darwin";
    digga = {
      url = "github:divnix/digga/home-manager-22.11";
      inputs.home-manager.follows = "home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager.url = "github:nix-community/home-manager";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils.url = "github:numtide/flake-utils";

    agenix.url = "github:montchr/agenix/darwin-support";
    deploy-rs.url = "github:serokell/deploy-rs";
    devshell.url = "github:numtide/devshell";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    gitignore.url = "github:hercules-ci/gitignore.nix";
    nix-dram = {
      url = "github:dramforever/nix-dram";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    nix-colors.url = "github:Misterio77/nix-colors";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nixos-generators.url = "github:nix-community/nixos-generators";
    nixago = {
      url = "github:nix-community/nixago";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nvfetcher.url = "github:berberman/nvfetcher";
    prefmanager.url = "github:malob/prefmanager";
    sops-nix.url = "github:Mic92/sops-nix";
    rnix-lsp.url = "github:nix-community/rnix-lsp";

    # TODO: use nix-colors or something
    base16-kitty = {
      url = "github:kdrag0n/base16-kitty";
      flake = false;
    };
  };

  outputs = inputs @ {
    self,
    flake-parts,
    digga,
    flake-utils,
    ...
  }: let
    inherit (digga.lib) flattenTree rakeLeaves;

    supportedSystems = with flake-utils.lib.system; [
      x86_64-linux
      aarch64-linux
      x86_64-darwin
      aarch64-darwin
    ];

    lib = inputs.nixos-unstable.lib.extend (lfinal: lprev: {
      ##: exo :: from without
      exo = builtins.mapAttrs (n: v: v.lib) {
        inherit
          (inputs)
          digga
          flake-parts
          gitignore
          nix-colors
          ;
        hm = inputs.home-manager;
        utils = inputs.flake-utils;
      };
    });

    peers = import ./ops/metadata/peers.nix;
    sharedModules = flattenTree (rakeLeaves ./modules);
    sharedProfiles = rakeLeaves ./profiles;
  in (flake-parts.lib.mkFlake {inherit self;} {
    systems = supportedSystems;
    perSystem = {
      inputs',
      config,
      ...
    }: {
      _module.args.pkgs = inputs'.nixpkgs.legacyPackages;
      _module.args.lib = config.lib;
    };
    imports = [
      {
        _module.args.peers = peers;
      }
      ./devShells
      ./nixpkgs
      ./packages
      ./darwin/packages

      ./darwin/configurations.nix
      ./home/configurations.nix
      ./nixos/configurations.nix
    ];
    flake = {
      inherit sharedModules sharedProfiles;

      overlays.default = final: prev: (import ./packages/all-packages.nix final);
      lib = import ./lib {inherit lib peers;};

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
