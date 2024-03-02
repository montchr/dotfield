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
}: let
  inherit (flake) inputs;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  l = lib // builtins;
  inputFlakes = l.filterAttrs (_: v: v ? outputs) inputs;
  inputsToPaths = l.mapAttrs' (n: v: {
    name = "nix/inputs/${n}";
    value.source = v.outPath;
  });
in {
  imports = [
    ./substituters/common.nix
    ./substituters/nixpkgs-update.nix
  ];

  environment.etc = inputsToPaths inputs;
  environment.systemPackages = [config.nix.package];

  nix = {
    package = pkgs.nixVersions.stable;
    nixPath = [
      "nixpkgs=${pkgs.path}"
      "home-manager=${inputs.home-manager}"
      "darwin=${inputs.darwin}"
      "/etc/nix/inputs"
    ];
    registry = l.mapAttrs (_: input: {flake = input;}) inputFlakes;
    settings = {
      # Builds have recently become unusably interrupted on Darwin
      # <https://github.com/NixOS/nix/issues/7273>
      auto-optimise-store = !isDarwin;
      builders-use-substitutes = true;
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
