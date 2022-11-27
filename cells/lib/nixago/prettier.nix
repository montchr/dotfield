# SPDX-FileCopyrightText: 2022 Chris Montgomery <chris@cdom.io>
#
# SPDX-License-Identifier: GPL-3.0-or-later
{
  inputs,
  cell,
}: let
  pkgs' = inputs.nixpkgs;
in {
  output = ".prettierrc.json";
  configData = {};
  format = "json";
  packages = [pkgs'.nodePackages.prettier];
}
