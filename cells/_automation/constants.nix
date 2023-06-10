# SPDX-FileCopyrightText: 2022-2023 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
{
  inputs,
  cell,
}: let
  inherit (inputs.apparat.lib) enumAttrs;
in {
  devshellCategories = enumAttrs [
    "dotfield"
    "general"
    "legal"
    "maintenance"
    "utils"
  ];
}
