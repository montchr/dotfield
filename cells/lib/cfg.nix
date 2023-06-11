# SPDX-FileCopyrightText: 2022-2023 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
{
  inputs,
  cell,
}: let
  inherit (inputs) haumea;
  l = inputs.nixpkgs.lib // builtins;
in
  l.mapAttrs (_: inputs.std.lib.dev.mkNixago)
  (haumea.lib.load {
    src = ./cfg;
    inputs = {inherit inputs;};
  })
