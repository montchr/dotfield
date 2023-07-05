# SPDX-FileCopyrightText: 2023 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
# Source: <https://github.com/divnix/std/blob/3e623646d47ec277947c9626d595f43042b94ab9/src/lib/cfg/editorconfig.nix>
{
  inputs,
  cell,
}: let
  l = nixpkgs.lib // builtins;
  inherit (inputs) nixpkgs;
in {
  data = {};
  output = ".editorconfig";
  engine = request: let
    inherit (request) data output;
    name = l.baseNameOf output;
    value = {
      globalSection = {root = data.root or true;};
      sections = l.removeAttrs data ["root"];
    };
  in
    nixpkgs.writeText name (l.generators.toINIWithGlobalSection {} value);
  packages = [nixpkgs.editorconfig-checker];
}
