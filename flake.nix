{
  description = "Dotfield";

  outputs = {
    nixpkgs,
    flake-parts,
    ...
  } @ inputs: let
    ops = import ./ops {inherit (inputs) haumea;};
  in (flake-parts.lib.mkFlake {inherit inputs;} {
    systems = ["aarch64-darwin" "aarch64-linux" "x86_64-darwin" "x86_64-linux"];
    std.grow.cellsFrom = ./cells;
    std.grow.cellBlocks = with inputs.std.blockTypes; [
      (data "constants")
      (data "data")
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
    imports = [
      inputs.std.flakeModule

      {_module.args = {inherit ops;};}

      ./flake-modules/homeConfigurations.nix

      ./lib
      ./packages

      ./machines/darwinConfigurations.nix
      ./machines/nixosConfigurations.nix
      ./users/homeConfigurations.nix
    ];
    perSystem = {
      system,
      inputs',
      ...
    }: {
      _module.args = {
        inherit ops;
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };
      };
      formatter = inputs'.nixpkgs.legacyPackages.alejandra;
    };
  });

  ##: channels
  inputs.nixpkgs.follows = "nixos-unstable";
  inputs.nixos-stable.url = "github:NixOS/nixpkgs/nixos-22.11";
  inputs.nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.nixpkgs-trunk.url = "github:NixOS/nixpkgs/master";

  inputs.hive = {
    url = "github:divnix/hive";
    inputs.colmena.follows = "colmena";
    inputs.disko.follows = "disko";
    inputs.home-manager.follows = "home-manager";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.paisano.follows = "std/paisano";
    # TODO: should exist?
    # inputs.darwin.follows = "darwin";
  };

  ##: core modules+libraries
  inputs.apparat.url = "sourcehut:~montchr/apparat";
  inputs.haumea.follows = "apparat/haumea";
  inputs.darwin.url = "github:LnL7/nix-darwin";
  inputs.devshell.url = "github:numtide/devshell";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.home-manager.url = "github:nix-community/home-manager";
  inputs.home-manager-gpg-agent-darwin.url = "github:montchr/home-manager/gpg-agent-darwin";
  inputs.nix-std.url = "github:chessai/nix-std";
  inputs.srvos.url = "github:numtide/srvos";
  inputs.std.url = "github:divnix/std";

  ##: customisation
  inputs.base16-schemes.url = "github:montchr/nix-base16-schemes";
  inputs.firefox-addons.url = "github:seadome/firefox-addons";
  inputs.iosevka-xtal.url = "github:montchr/iosevka-xtal";

  ##: work
  inputs.klein-infra.url = "github:kleinweb/infra";

  ##: universal
  inputs.agenix.url = "github:ryantm/agenix";
  inputs.deadnix.url = "github:astro/deadnix";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.sops-nix.url = "github:Mic92/sops-nix";
  inputs.nil-lsp.url = "github:oxalica/nil";

  ##: darwin-specific
  inputs.prefmanager.url = "github:malob/prefmanager";

  ##: linux-specific
  inputs.colmena.url = "github:zhaofengli/colmena";
  inputs.disko.url = "github:nix-community/disko";
  inputs.microvm.url = "github:astro/microvm.nix";
  inputs.nixos-hardware.url = "github:nixos/nixos-hardware";
  inputs.nixos-generators.url = "github:nix-community/nixos-generators";
  inputs.nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland";

  ##: emacs
  inputs.emacs-overlay.url = "github:nix-community/emacs-overlay";

  ##: et cetera
  inputs.apparat.inputs.std.follows = "std";
  inputs.darwin.inputs.nixpkgs.follows = "nixpkgs";
  inputs.disko.inputs.nixpkgs.follows = "nixpkgs";
  inputs.home-manager.inputs.nixpkgs.follows = "nixpkgs";
  inputs.home-manager-gpg-agent-darwin.inputs.nixpkgs.follows = "nixpkgs";
  inputs.emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
  inputs.klein-infra.inputs.nixpkgs.follows = "nixpkgs";
  inputs.klein-infra.inputs.dmerge.follows = "std/dmerge";
  inputs.klein-infra.inputs.std.follows = "std";
  inputs.microvm.inputs.nixpkgs.follows = "nixpkgs";
  inputs.base16-schemes.inputs.std.follows = "std";
  inputs.base16-schemes.inputs.nixpkgs.follows = "nixpkgs";
  inputs.nixpkgs-wayland.inputs.nixpkgs.follows = "nixpkgs";
  inputs.std.inputs.nixpkgs.follows = "nixpkgs";

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
