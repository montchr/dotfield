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

        # XXX: Required for Zotero: https://github.com/NixOS/nixpkgs/issues/568692#issuecomment-6189832238
        #      See also: https://github.com/NixOS/nixpkgs/issues/579749#issuecomment-6764501530
        permittedInsecurePackages = [
          "firefox-esr-140-bin-unwrapped-140.17.0esr"
        ];
      };
  };
}
