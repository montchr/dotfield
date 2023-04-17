# SPDX-FileCopyrightText: 2022-2023 Chris Montgomery <chris@cdom.io>
#
# SPDX-License-Identifier: GPL-3.0-or-later
{
  inputs,
  cell,
} @ cellArgs:
builtins.mapAttrs (_: inputs.std.lib.dev.mkNixago) {
  commitlint = import ./cfg/commitlint.nix cellArgs;
  garnix = import ./cfg/garnix.nix cellArgs;
  prettier = import ./cfg/prettier.nix cellArgs;
  statix = import ./cfg/statix.nix cellArgs;
  stylua = import ./cfg/stylua.nix cellArgs;
}
