# SPDX-FileCopyrightText: 2022 Chris Montgomery <chris@cdom.io>
#
# SPDX-License-Identifier: GPL-3.0-or-later
{
  inputs,
  cell,
} @ cellArgs:
builtins.mapAttrs (_: inputs.std.lib.dev.mkNixago) {
  commitlint = import ./nixago/commitlint.nix cellArgs;
  # lint-staged = import ./nixago/lint-staged.nix cellArgs;
  prettier = import ./nixago/prettier.nix cellArgs;
  statix = import ./nixago/statix.nix cellArgs;
}
