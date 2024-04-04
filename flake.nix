{
  description = "Dotfield";

  outputs =
    {
      nixpkgs,
      flake-parts,
      haumea,
      namaka,
      ...
    }@inputs:
    (
      let
        ops = import ./ops/data.nix { inherit haumea; };
      in
      flake-parts.lib.mkFlake { inherit inputs; } {
        systems = [
          "aarch64-darwin"
          "aarch64-linux"
          "x86_64-darwin"
          "x86_64-linux"
        ];
        imports = [
          inputs.devshell.flakeModule

          {
            _module.args = {
              inherit ops;
            };
          }

          ./flake-modules/homeConfigurations.nix

          ./lib
          ./packages
          ./nixos
          ./home
          ./darwin

          ./ops/devshells

          ./hive.nix
        ];

        flake.checks = namaka.lib.load {
          src = ./tests;
          inputs = {
            inherit ops;
          };
        };

        perSystem =
          { system, ... }:
          {
            _module.args = {
              inherit ops;
              pkgs = import nixpkgs {
                inherit system;
                config.allowUnfree = true;
              };
            };
            # FIXME: currently results in cross-compiling due to haskell's IFD!
            #        must wait for this to arrive in a binary cache in order to
            #        use `nix fmt`
            # formatter = inputs'.nixfmt.packages.default;
          };
      }
    );

  inputs = {

    ##: channels
    nixpkgs.follows = "nixos-unstable";
    nixpkgs.inputs.nixpkgs.follows = "nixos-unstable";
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-stable.url = "github:NixOS/nixpkgs/nixos-23.11";
    nixpkgs-trunk.url = "github:NixOS/nixpkgs/master";

    ##: core modules+libraries
    apparat.url = "sourcehut:~montchr/apparat";
    attic.url = "github:zhaofengli/attic";
    haumea.url = "github:nix-community/haumea";
    darwin.url = "github:LnL7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    devshell.url = "github:numtide/devshell";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    home-manager-gpg-agent-darwin.url = "github:montchr/home-manager/gpg-agent-darwin";
    home-manager-gpg-agent-darwin.inputs.nixpkgs.follows = "nixpkgs";
    nixos-apple-silicon.url = "github:tpwrules/nixos-apple-silicon";
    asahi-tuvok-firmware.url = "git+ssh://git@git.sr.ht/~montchr/asahi-tuvok-firmware";
    simple-nixos-mailserver.url = "gitlab:simple-nixos-mailserver/nixos-mailserver/nixos-23.05";
    # FIXME: remove -- will break due to branch pinning above
    simple-nixos-mailserver.inputs.nixpkgs.follows = "nixos-stable";
    srvos.url = "github:numtide/srvos";

    ##: ops
    colmena.url = "github:zhaofengli/colmena";
    sops-nix.url = "github:Mic92/sops-nix";
    namaka = {
      url = "github:nix-community/namaka";
      inputs = {
        haumea.follows = "haumea";
        nixpkgs.follows = "nixpkgs";
      };
    };

    ##: customisation
    base16-schemes.url = "github:montchr/nix-base16-schemes";
    base16-schemes.inputs.nixpkgs.follows = "nixpkgs";
    firefox-addons.url = "sourcehut:~montchr/firefox-addons";
    iosevka-xtal.url = "github:montchr/iosevka-xtal";
    seadome-wallpapers.url = "sourcehut:~montchr/wallpapers";

    ##: apps/tools
    deadnix.url = "github:astro/deadnix";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    nil-lsp.url = "github:oxalica/nil";
    nix-index-database.url = "github:Mic92/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";
    nixfmt.url = "github:NixOS/nixfmt/master";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.flake-utils.follows = "flake-utils";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    pre-commit-hooks.inputs.nixpkgs-stable.follows = "nixos-stable";

    ##: system
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";
    kmonad.url = "git+https://github.com/kmonad/kmonad?submodules=1&dir=nix";
    microvm.url = "github:astro/microvm.nix";
    microvm.inputs.nixpkgs.follows = "nixpkgs";
    musnix.url = "github:musnix/musnix";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nixos-generators.url = "github:nix-community/nixos-generators";
    nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland";
    nixpkgs-wayland.inputs.nixpkgs.follows = "nixpkgs";
    prefmanager.url = "github:malob/prefmanager";
  };

  # NOTE: Retained for provisioning purposes, but normally unnecessary.
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
