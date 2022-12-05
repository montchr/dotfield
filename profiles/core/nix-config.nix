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
  inputFlakes = l.filterAttrs (_: v: v ? outputs) inputs;
  inputsToPaths = l.mapAttrs' (n: v: {
    name = "nix/inputs/${n}";
    value.source = v.outPath;
  });
in {
  imports = [./substituters/common.nix];

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
