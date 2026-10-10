# SPDX-FileCopyrightText: (C) 2026 Chris Montgomery <chmont@protonmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

{
  imports = [
    ./__beets-packages.nix
    ./__by-name.nix
  ];

  perSystem =
    { pkgs, system, ... }:
    {
      packages = {

      };
    };
}
