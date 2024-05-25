# Copyright (C) 2022-2024 Chris Montgomery
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
  inputFlakes = lib.filterAttrs (_: v: v ? outputs) inputs;
  inputsToPaths = lib.mapAttrs' (
    n: v: {
      name = "nix/inputs/${n}";
      value.source = v.outPath;
    }
  );
in

{
  imports = [ ./__substituters.nix ];

  environment.etc = inputsToPaths inputs;
  environment.systemPackages = [ config.nix.package ];

  nix = {
    nixPath = [
      "nixpkgs=${pkgs.path}"
      "home-manager=${inputs.home-manager}"
      "/etc/nix/inputs"
    ];
    registry = lib.mapAttrs (_: input: { flake = input; }) inputFlakes;
    settings = {
      allowed-users = [ "*" ];
      trusted-users = [
        "root"
        "@wheel"
      ];
      auto-optimise-store = lib.mkDefault true;
      builders-use-substitutes = true;
      sandbox = lib.mkDefault true;
      # TODO: always appropriate??
      system-features = [
        "nixos-test"
        "benchmark"
        "big-parallel"
        "kvm"
      ];
      experimental-features = [
        "nix-command"
        "flakes"
      ];
    };
    gc.dates = "weekly";
    optimise.automatic = true;
    gc.automatic = lib.mkDefault (!config.programs.nh.clean.enable);

    extraOptions = ''
      warn-dirty = false
    '';
  };
}
