{
  description = "Dotfield";

  # Enable as needed for bootstrapping. Otherwise these entries cause warnings on every rebuild.
  # nixConfig.extra-experimental-features = "nix-command flakes";
  # nixConfig.extra-substituters = "https://iosevka-xtal.cachix.org https://dotfield.cachix.org https://nixpkgs-wayland.cachix.org https://nix-community.cachix.org";
  # nixConfig.extra-trusted-public-keys = "iosevka-xtal.cachix.org-1:5d7Is01fs3imwU9w5dom2PcSskJNwtJGbfjRxunuOcw= dotfield.cachix.org-1:b5H/ucY/9PDARWG9uWA87ZKWUBU+hnfF30amwiXiaNk= nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=";

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
    home-manager = {
      url = "github:nix-community/home-manager";
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

    priorityOverlays = [
      # FIXME: remove after https://github.com/NixOS/nixpkgs/pull/193589
      # [2022-10-14]: waiting for review since 2022-10-01
      self.overlays.nixpkgs-193589-jemalloc
    ];

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
    perSystem = {
      system,
      inputs',
      ...
    }: let
      pkgsets = {
        default = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = priorityOverlays ++ exoOverlays ++ esoOverlays;
        };
        stable = inputs'.nixos-stable.legacyPackages;
        unstable = inputs'.nixos-unstable.legacyPackages;
        trunk = inputs'.nixpkgs-trunk.legacyPackages;
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
}
