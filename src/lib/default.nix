# SPDX-FileCopyrightText: (C) 2026 Chris Montgomery <chmont@protonmail.com>
# SPDX-License-Identifier: GPL-3.0-or-later

{
  config,
  lib,
  self,
  ...
}:
{
  flake.lib = {
    nixpkgsConfig =
      let
        inherit (builtins) elem;
        inherit (lib) getName;

        allowUnfreePackages = [
          "berkeley-mono-typeface.zip"
          "citrix-secure-access"
        ];
      in
      {
        inherit allowUnfreePackages;

        overlays = [ config.overlays.default ];

        allowUnfreePredicate = pkg: elem (getName pkg) allowUnfreePackages;
      };
  };
}
