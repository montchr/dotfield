# SPDX-FileCopyrightText: (C) 2026 Chris Montgomery <chmont@protonmail.com>
# SPDX-License-Identifier: GPL-3.0-or-later
{ lib, ... }:
let
  inherit (builtins) map;
  inherit (lib) replaceString;

  withSearchAliasPrefix = s: "@" + (replaceString "@" "" s);
  withAlias = alias: attrs: attrs // { definedAliases = (map withSearchAliasPrefix [ alias ]); };
  withAliases = aliases: attrs: attrs // { definedAliases = (map withSearchAliasPrefix aliases); };

  engine = template: { urls = [ { inherit template; } ]; };
in
{
  flake.lib.firefox = {
    inherit engine;

    engine' = alias: template: withAlias alias (engine template);

    engineWithAliases = aliases: template: withAliases aliases (engine template);
  };
}
