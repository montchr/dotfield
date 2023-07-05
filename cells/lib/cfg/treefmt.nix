# SPDX-FileCopyrightText: 2023 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
# Source: <https://github.com/divnix/std/blob/3e623646d47ec277947c9626d595f43042b94ab9/src/lib/cfg/treefmt.nix>
{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
in {
  data = {};
  output = "treefmt.toml";
  format = "toml";
  commands = [{package = nixpkgs.treefmt;}];
}
