# Copyright (C) 2022-2025 Chris Montgomery
# Copyright (C) 2021 Gytis Ivaskevicius
# SPDX-License-Identifier: GPL-3.0-or-later AND MIT
# <https://github.com/gytis-ivaskevicius/flake-utils-plus/blob/2bf0f91643c2e5ae38c1b26893ac2927ac9bd82a/LICENSE>
{
  flake,
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (flake) inputs;
  cfg = config.nix;
  inputFlakes = lib.filterAttrs (n: v: (v ? outputs) && n != "nixpkgs") inputs;
  inputsToPaths = lib.mapAttrs' (
    n: v: {
      name = "nix/inputs/${n}";
      value.source = v.outPath;
    }
  );
in

{
  environment.etc = inputsToPaths inputs;
  environment.systemPackages = [ cfg.package ];

  nix = {
    nixPath = [
      "nixpkgs=${pkgs.path}"
      "home-manager=${inputs.home-manager}"
      "/etc/nix/inputs"
    ];
    distributedBuilds = true;
    registry = builtins.removeAttrs (lib.mapAttrs (_: input: { flake = input; }) inputFlakes) [
      "nixpkgs"
    ];
    settings = {
      allowed-users = [ "*" ];
      trusted-users = [
        "root"
        "@wheel"
      ];

      ## === Features ===

      experimental-features = [
        "nix-command"
        "flakes"
      ];

      # TODO: always appropriate??
      system-features = [
        "nixos-test"
        "benchmark"
        "big-parallel"
        "kvm"
      ];

      ## === Substituters ===

      builders-use-substitutes = true;
      substituters = [
        "https://dotfield.cachix.org"
        "https://nix-community.cachix.org"
      ];
      trusted-substituters = [
        "ssh://eu.nixbuild.net"
        "https://nixpkgs-update.cachix.org/"
      ];
      trusted-public-keys = [
        "dotfield.cachix.org-1:b5H/ucY/9PDARWG9uWA87ZKWUBU+hnfF30amwiXiaNk="
        "nixbuild.net/cdom-1:DU7hcG2k5kj9nC6NUvsOYQNiaI5UXYjjY5gBOccaND4="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "nixpkgs-update.cachix.org-1:6y6Z2JdoL3APdu6/+Iy8eZX2ajf09e4EE9SnxSML1W8="
      ];
    };

    ## === Store Maintenance ===

    ## `nix store optimise`
    settings.auto-optimise-store = lib.mkDefault true;
    optimise.automatic = !cfg.settings.auto-optimise-store;

    ## `nix store gc`
    gc.dates = lib.mkDefault "weekly";
    gc.automatic = lib.mkDefault true;

  };
}
