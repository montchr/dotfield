# SPDX-FileCopyrightText: 2022 Chris Montgomery <chris@cdom.io>
#
# SPDX-License-Identifier: GPL-3.0-or-later
{
  inputs,
  cell,
}: let
  l = inputs.nixpkgs.lib // builtins;
in {
  /*
  Functional sugar for a simple conditional ternary statement.

  Type: Bool -> Any -> Any

  Example:
    let foo = "bar"; in
    tern (foo == "bar") "fnord" "word"
    => "fnord"
  */
  tern = p: y: n:
    if p
    then y
    else n;

  /*
  Convert a list of strings to an attrset where the keys match the values.

  Example:
    enumAttrs ["foo" "bar"]
    => {foo = "foo"; bar = "bar";}
  */
  enumAttrs = enum: l.genAttrs enum (s: s);
}
