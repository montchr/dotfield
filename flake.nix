{
  description = "Dotfield";

  outputs =
    inputs@{ nixpkgs, flake-parts, ... }:
    (flake-parts.lib.mkFlake { inherit inputs; } (
      { config, ... }:
      {
        debug = true;

        systems = [
          "aarch64-linux"
          "x86_64-linux"
        ];

        imports = [
          inputs.flake-parts.flakeModules.modules
          inputs.git-hooks.flakeModule

          ./src
          ./dev
          ./tests

          ./hive.nix
        ];

        perSystem =
          { system, pkgs, ... }:
          {
            _module.args = {
              pkgs = import nixpkgs {
                inherit system;
                config.allowUnfree = true;
              };
            };
            formatter = pkgs.nixfmt;
          };
      }
    ));

  inputs = {

    ##: channels
    nixpkgs.follows = "nixos-unstable";
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    # nixos-unstable.url = "github:astratagem/nixpkgs/nixos-unstable";
    nixos-stable.url = "github:NixOS/nixpkgs/nixos-26.05";
    nixpkgs-trunk.url = "github:NixOS/nixpkgs/master";
    nixpkgs-apple-silicon.follows = "nixos-apple-silicon/nixpkgs";

    ##: core libraries
    apparat.url = "git+ssh://git@codeberg.org/astratagem/apparat.git";
    dmerge = {
      url = "github:divnix/dmerge";
      inputs.haumea.follows = "haumea";
      inputs.nixlib.follows = "nixpkgs-lib";
    };
    haumea.url = "github:nix-community/haumea";
    import-tree.url = "github:denful/import-tree";
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs-lib.follows = "flake-parts/nixpkgs-lib";

    ##: core modules
    beams.url = "github:kleinweb/beams";
    beams.inputs.nixpkgs.follows = "nixpkgs";
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixos-unstable";

    ##: hardware
    nixos-apple-silicon = {
      # url = "github:nix-community/nixos-apple-silicon";
      url = "github:astratagem/nixos-apple-silicon/dev";
      inputs.nixpkgs.follows = "nixos-unstable";
    };
    asahi-tuuvok-firmware.url = "git+ssh://git@codeberg.org/astratagem/asahi-tuuvok-firmware.git";
    nixos-hardware.url = "github:NixOS/nixos-hardware";

    ##: ops
    colmena.url = "github:zhaofengli/colmena";
    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-unit = {
      url = "github:nix-community/nix-unit";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix.url = "github:Mic92/sops-nix";

    ##: customisation
    base16-schemes.url = "github:astratagem/nix-base16-schemes";
    base16-schemes.inputs.nixpkgs.follows = "nixpkgs";
    ironbar.url = "github:JakeStanger/ironbar?ref=v0.19.0";
    ironbar.inputs.nixpkgs.follows = "nixpkgs";
    stylix.url = "github:danth/stylix";

    ##: apps/tools
    ceamx = {
      url = "git+ssh://git@codeberg.org/astratagem/ceamx.git";
      inputs.nixpkgs.follows = "nixos-stable";
      inputs.apparat.follows = "apparat";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    llm-agents.url = "github:numtide/llm-agents.nix";
    nix-flatpak.url = "github:gmodena/nix-flatpak?ref=latest";
    nix-index-database.url = "github:Mic92/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";

    ##: system
    musnix.url = "github:musnix/musnix";
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
}
