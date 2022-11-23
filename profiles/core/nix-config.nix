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
  lib,
  pkgs,
  ...
}: let
  inherit (self) inputs;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  l = lib // builtins;
  substituters = [
    ##: personal
    "https://dotfield.cachix.org"
    "https://iosevka-xtal.cachix.org"
    ##: community
    "https://nixpkgs-wayland.cachix.org"
    "https://nix-community.cachix.org"
    ##: official
    "https://cache.nixos.org/"
  ];
  trusted-public-keys = [
    "dotfield.cachix.org-1:b5H/ucY/9PDARWG9uWA87ZKWUBU+hnfF30amwiXiaNk="
    "iosevka-xtal.cachix.org-1:5d7Is01fs3imwU9w5dom2PcSskJNwtJGbfjRxunuOcw="
    "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
  ];
  trusted-substituters = substituters;
  inputFlakes = l.filterAttrs (_: v: v ? outputs) inputs;
  inputsToPaths = l.mapAttrs' (n: v: {
    name = "nix/inputs/${n}";
    value.source = v.outPath;
  });
in {
  environment.etc = inputsToPaths inputs;
  nix = {
    package = pkgs.nix;
    nixPath = [
      "nixpkgs=${pkgs.path}"
      "home-manager=${inputs.home-manager}"
      "darwin=${inputs.darwin}"
      "/etc/nix/inputs"
    ];
    registry = l.mapAttrs (_: flake: {inherit flake;}) inputFlakes;
    settings = {
      inherit
        substituters
        trusted-substituters
        trusted-public-keys
        ;
      auto-optimise-store = true;
      experimental-features = ["nix-command" "flakes"];
      sandbox = l.mkDefault (!isDarwin);
      allowed-users = ["*"];
      trusted-users = ["root" "@wheel"];
    };

    gc.automatic = true;

    extraOptions = ''
      warn-dirty = false
    '';
  };
}
