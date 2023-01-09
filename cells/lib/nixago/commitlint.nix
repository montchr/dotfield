# SPDX-FileCopyrightText: 2022-2023 Chris Montgomery <chris@cdom.io>
#
# SPDX-License-Identifier: GPL-3.0-or-later
{
  inputs,
  cell,
}: let
  pkgs' = inputs.nixpkgs;
in {
  configData = {};
  output = ".commitlintrc.json";
  format = "json";
  packages = [pkgs'.commitlint];
}
