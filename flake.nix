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

    darwin.url = "github:LnL7/nix-darwin";
    home-manager.url = "github:nix-community/home-manager";

    ##: --- utilities ----------------------------------------------------------

    agenix.url = "github:montchr/agenix/darwin-support";
    deploy-rs.url = "github:serokell/deploy-rs";
    devshell.url = "github:numtide/devshell";
    flake-utils.url = "github:numtide/flake-utils";
    prefmanager.url = "github:malob/prefmanager";
    nixos-generators.url = "github:nix-community/nixos-generators";
    nvfetcher.url = "github:berberman/nvfetcher";
    sops-nix.url = "github:Mic92/sops-nix";

    digga.url = "github:divnix/digga/home-manager-22.11";
    digga.inputs.home-manager.follows = "home-manager";
    digga.inputs.nixpkgs.follows = "nixpkgs";

    nix-dram = {
      url = "github:dramforever/nix-dram";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    nixago = {
      url = "github:nix-community/nixago";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ##: --- sources ------------------------------------------------------------

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    gitignore.url = "github:hercules-ci/gitignore.nix";
    nix-colors.url = "github:Misterio77/nix-colors";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nur.url = "github:nix-community/NUR";
    rnix-lsp.url = "github:nix-community/rnix-lsp";

    base16-kitty = {
      url = "github:kdrag0n/base16-kitty";
      flake = false;
    };

    nixpkgs-wayland = {
      url = "github:nix-community/nixpkgs-wayland";
      # Packages are built against this channel.
      inputs.nixpkgs.follows = "nixos-unstable";
    };
  };

  outputs = {
    self,
    flake-parts,
    nixos-unstable,
    digga,
    flake-utils,
    ...
  }: let
    inherit (digga.lib) flattenTree rakeLeaves;
    inherit (lib.dotfield) importLeaves;

    supportedSystems = with flake-utils.lib.system; [
      x86_64-linux
      aarch64-linux
      x86_64-darwin
      aarch64-darwin
    ];

    lib = nixos-unstable.lib.extend (lfinal: lprev: {
      digga = digga.lib;
      dotfield = import ./lib {
        inherit peers;
        lib = lfinal;
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
      # ./darwin/flake-module.nix
      # ./darwin/packages

      ./shell.nix
      ./darwin/configurations.nix
      ./home/configurations.nix
      ./nixos/configurations.nix
    ];
    flake = {
      inherit lib sharedModules sharedProfiles;

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
