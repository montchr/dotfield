# SPDX-FileCopyrightText: 2022 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
{
  inputs,
  cell,
}: let
  # inherit (inputs) nixpkgs;
  inherit (inputs.cells.lib.functions) enumAttrs;
  # l = inputs.nixpkgs.lib // builtins;
in {
  devshellCategories = enumAttrs [
    "dotfield"
  ];
}
