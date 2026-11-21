# SPDX-FileCopyrightText: (C) 2026 Chris Montgomery <chmont@protonmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

{ inputs, ... }:
let
  inherit (inputs) import-tree;
in
{
  imports = [
    (import-tree [
      ./lib
      ./features
      ./hosts
      ./overlays
      ./users
    ])

    ./meta
    ./modules
    ./packages
  ];
}
