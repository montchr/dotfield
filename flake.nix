{
  description = "Dotfield";

  outputs =
    {
      nixpkgs,
      nixos-stable,
      nixos-unstable,
      nixpkgs-trunk,
      nixpkgs-wayland,
      flake-parts,
      haumea,
      ...
    }@inputs:
    (
      let
        ops = import ./ops/data.nix { inherit haumea; };
      in
      flake-parts.lib.mkFlake { inherit inputs; } {
        debug = true;

        systems = [
          "aarch64-linux"
          "x86_64-linux"
        ];

        imports = [
          inputs.devshell.flakeModule
          inputs.pre-commit-hooks.flakeModule

          {
            _module.args = {
              inherit ops;
            };
          }

          ./flake-modules/homeConfigurations.nix

          ./src

          ./home

          ./dev/git-hooks
          ./dev/shells

          ./hive.nix
        ];

        perSystem =
          { system, pkgs, ... }:
          {
            _module.args = {
              inherit ops;
              pkgs = import nixpkgs {
                inherit system;
                config.allowUnfree = true;
                overlays = (import ./overlays/default.nix { inherit inputs; });
              };
            };
            formatter = pkgs.nixfmt-rfc-style;
          };
      }
    );

  inputs = {

    ##: channels
    nixpkgs.follows = "nixos-unstable";
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    # nixos-unstable.url = "github:montchr/nixpkgs/nixos-unstable";
    nixos-stable.url = "github:NixOS/nixpkgs/nixos-25.05";
    nixpkgs-trunk.url = "github:NixOS/nixpkgs/master";
    nixpkgs-apple-silicon.follows = "nixos-apple-silicon/nixpkgs";
    nixpkgs-for-beets-not-failing-build.url = "github:NixOS/nixpkgs?rev=910796cabe436259a29a72e8d3f5e180fc6dfacc";

    ##: core libraries
    apparat.url = "sourcehut:~montchr/apparat";
    haumea.url = "github:nix-community/haumea";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils.url = "github:numtide/flake-utils";
    globset = {
      url = "github:pdtpartners/globset";
      inputs.nixpkgs-lib.follows = "nixpkgs-lib";
    };
    nixpkgs-lib.follows = "flake-parts/nixpkgs-lib";

    ##: core modules
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixos-unstable";
    # FIXME: update
    simple-nixos-mailserver.url = "gitlab:simple-nixos-mailserver/nixos-mailserver/nixos-23.05";
    simple-nixos-mailserver.inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    srvos.url = "github:numtide/srvos";

    ##: hardware
    nixos-apple-silicon.url = "github:nix-community/nixos-apple-silicon";
    asahi-tuvok-firmware.url = "git+ssh://git@git.sr.ht/~montchr/asahi-tuvok-firmware";
    nixos-hardware.url = "github:NixOS/nixos-hardware";

    ##: ops
    attic.url = "github:zhaofengli/attic";
    colmena.url = "github:zhaofengli/colmena";
    devshell.url = "github:numtide/devshell";
    sops-nix.url = "github:Mic92/sops-nix";

    ##: customisation
    base16-schemes.url = "github:montchr/nix-base16-schemes";
    base16-schemes.inputs.nixpkgs.follows = "nixpkgs";
    seadome-wallpapers.url = "sourcehut:~montchr/wallpapers";
    stylix.url = "github:danth/stylix";
    wrapper-manager.url = "github:viperML/wrapper-manager";

    ##: apps/tools
    ceamx = {
      url = "github:montchr/ceamx";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.apparat.follows = "apparat";
      inputs.devshell.follows = "devshell";
    };
    deadnix.url = "github:astro/deadnix";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    nix-ai-tools.url = "github:numtide/nix-ai-tools";
    nix-inspect.url = "github:bluskript/nix-inspect";
    nix-index-database.url = "github:Mic92/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    prj-spec = {
      url = "github:kleinweb/prj-spec/contrib-shell-hook?dir=contrib";
      flake = false;
    };

    ##: system
    microvm.url = "github:astro/microvm.nix";
    microvm.inputs.nixpkgs.follows = "nixpkgs";
    musnix.url = "github:musnix/musnix";
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland";
    nixpkgs-wayland.inputs.nixpkgs.follows = "nixpkgs";
  };

  # # NOTE: Retained for provisioning purposes, but normally unnecessary.
  # nixConfig = {
  #   extra-experimental-features = "nix-command flakes";
  #   extra-substituters = [
  #     "https://dotfield.cachix.org"
  #     "https://nix-community.cachix.org"
  #   ];
  #   extra-trusted-public-keys = [
  #     "dotfield.cachix.org-1:b5H/ucY/9PDARWG9uWA87ZKWUBU+hnfF30amwiXiaNk="
  #     "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
  #   ];
  # };
}
