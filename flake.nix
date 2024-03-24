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

  ##: channels
  inputs.nixpkgs.follows = "nixos-unstable";
  inputs.nixpkgs.inputs.nixpkgs.follows = "nixos-unstable";
  inputs.nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.nixos-stable.url = "github:NixOS/nixpkgs/nixos-23.11";
  inputs.nixpkgs-trunk.url = "github:NixOS/nixpkgs/master";

  ##: core modules+libraries
  inputs.apparat.url = "sourcehut:~montchr/apparat";
  inputs.attic.url = "github:zhaofengli/attic";
  inputs.haumea.url = "github:nix-community/haumea";
  inputs.darwin.url = "github:LnL7/nix-darwin";
  inputs.devshell.url = "github:numtide/devshell";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.home-manager.url = "github:nix-community/home-manager";
  inputs.home-manager-gpg-agent-darwin.url = "github:montchr/home-manager/gpg-agent-darwin";
  inputs.nixos-apple-silicon.url = "github:tpwrules/nixos-apple-silicon";
  inputs.asahi-tuvok-firmware.url = "git+ssh://git@git.sr.ht/~montchr/asahi-tuvok-firmware";
  inputs.simple-nixos-mailserver.url = "gitlab:simple-nixos-mailserver/nixos-mailserver/nixos-23.05";
  inputs.srvos.url = "github:numtide/srvos";

  ##: ops
  inputs.colmena.url = "github:zhaofengli/colmena";
  inputs.sops-nix.url = "github:Mic92/sops-nix";
  inputs.namaka = {
    url = "github:nix-community/namaka";
    inputs = {
      haumea.follows = "haumea";
      nixpkgs.follows = "nixpkgs";
    };
  };

  ##: customisation
  inputs.base16-schemes.url = "github:montchr/nix-base16-schemes";
  inputs.firefox-addons.url = "sourcehut:~montchr/firefox-addons";
  inputs.iosevka-xtal.url = "github:montchr/iosevka-xtal";
  inputs.seadome-wallpapers.url = "sourcehut:~montchr/wallpapers";

  ##: apps/tools
  inputs.deadnix.url = "github:astro/deadnix";
  inputs.emacs-overlay.url = "github:nix-community/emacs-overlay";
  inputs.nil-lsp.url = "github:oxalica/nil";
  inputs.nix-index-database.url = "github:Mic92/nix-index-database";
  inputs.nixfmt.url = "github:NixOS/nixfmt/master";
  inputs.pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

  ##: system
  inputs.disko.url = "github:nix-community/disko";
  inputs.kmonad.url = "git+https://github.com/kmonad/kmonad?submodules=1&dir=nix";
  inputs.microvm.url = "github:astro/microvm.nix";
  inputs.musnix.url = "github:musnix/musnix";
  inputs.nixos-hardware.url = "github:nixos/nixos-hardware";
  inputs.nixos-generators.url = "github:nix-community/nixos-generators";
  inputs.nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland";
  inputs.prefmanager.url = "github:malob/prefmanager";

  ##: et cetera ad infinitum
  inputs.base16-schemes.inputs.nixpkgs.follows = "nixpkgs";
  inputs.darwin.inputs.nixpkgs.follows = "nixpkgs";
  inputs.disko.inputs.nixpkgs.follows = "nixpkgs";
  inputs.home-manager.inputs.nixpkgs.follows = "nixpkgs";
  inputs.home-manager-gpg-agent-darwin.inputs.nixpkgs.follows = "nixpkgs";
  inputs.emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
  inputs.microvm.inputs.nixpkgs.follows = "nixpkgs";
  inputs.nix-index-database.inputs.nixpkgs.follows = "nixpkgs";
  inputs.pre-commit-hooks.inputs.flake-utils.follows = "flake-utils";
  inputs.pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
  inputs.pre-commit-hooks.inputs.nixpkgs-stable.follows = "nixos-stable";
  inputs.nixpkgs-wayland.inputs.nixpkgs.follows = "nixpkgs";
  inputs.simple-nixos-mailserver.inputs.nixpkgs.follows = "nixos-stable";

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
