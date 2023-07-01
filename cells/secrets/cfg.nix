# SPDX-FileCopyrightText: 2023 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
{
  inputs,
  cell,
}: let
  inherit (inputs.cells) lib;
in {
  sops = lib.cfg.sops {
    data = import ./cfg/sops.nix {inherit inputs cell;};
  };
}
