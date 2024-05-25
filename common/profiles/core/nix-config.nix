# Copyright (C) 2022-2023 Chris Montgomery
# SPDX-License-Identifier: GPL-3.0+
#
# Copyright (C) 2021 Gytis Ivaskevicius
# SPDX-License-Identifier: MIT
#
## Sources:
#
# https://github.com/gytis-ivaskevicius/flake-utils-plus/blob/2bf0f91643c2e5ae38c1b26893ac2927ac9bd82a/LICENSE
{
  config,
  lib,
  pkgs,
  flake,
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
  imports = [
    ./substituters/common.nix
    ./substituters/nixpkgs-update.nix
  ];

  environment.etc = inputsToPaths inputs;
  environment.systemPackages = [ config.nix.package ];

  nix = {
    package = pkgs.nixVersions.stable;
    nixPath = [
      "nixpkgs=${pkgs.path}"
      "home-manager=${inputs.home-manager}"
      "/etc/nix/inputs"
    ];
    registry = lib.mapAttrs (_: input: { flake = input; }) inputFlakes;
    settings = {
      auto-optimise-store = lib.mkDefault true;
      builders-use-substitutes = true;
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      sandbox = lib.mkDefault true;
      allowed-users = [ "*" ];
      trusted-users = [
        "root"
        "@wheel"
      ];
    };

    gc.automatic = lib.mkDefault (!config.programs.nh.clean.enable);

    extraOptions = ''
      warn-dirty = false
    '';
  };
}
