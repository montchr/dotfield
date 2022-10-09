# Copyright (C) 2022 Chris Montgomery
# SPDX-License-Identifier: GPL-3.0+
#
# Copyright (C) 2021 Gytis Ivaskevicius
# SPDX-License-Identifier: MIT
#
## Sources:
#
# https://github.com/gytis-ivaskevicius/flake-utils-plus/blob/2bf0f91643c2e5ae38c1b26893ac2927ac9bd82a/LICENSE
{
  self,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit
    (builtins)
    mapAttrs
    ;
  inherit
    (lib)
    filterAttrs
    mapAttrs'
    ;
  inherit (self) inputs;
  substituters = [
    "https://cache.nixos.org/"
    "https://dotfield.cachix.org"
    "https://nix-community.cachix.org"
    "https://nixpkgs-wayland.cachix.org"
  ];
  trusted-substituters = substituters;
  inputFlakes = filterAttrs (_: v: v ? outputs) inputs;
  inputsToPaths = mapAttrs' (name: value: {
    name = "nix/inputs/${name}";
    value.source = value.outPath;
  });
in {
  environment.etc = inputsToPaths inputs;
  nix = {
    package = pkgs.nix;
    nixPath = [
      "nixpkgs=${pkgs.path}"
      "home-manager=${inputs.home-manager}"
      "/etc/nix/inputs"
    ];
    registry = mapAttrs (_: flake: {inherit flake;}) inputFlakes;
    settings = {
      inherit substituters trusted-substituters;
      auto-optimise-store = true;
      experimental-features = ["nix-command" "flakes"];
      sandbox = lib.mkDefault (!pkgs.stdenv.hostPlatform.isDarwin);
      allowed-users = ["*"];
      trusted-users = ["root" "@wheel" "@seadome"];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "dotfield.cachix.org-1:b5H/ucY/9PDARWG9uWA87ZKWUBU+hnfF30amwiXiaNk="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
      ];
    };

    gc = {
      automatic = true;
    };

    extraOptions = ''
      warn-dirty = false
    '';
  };
}
