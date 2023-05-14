# SPDX-FileCopyrightText: 2023 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
{
  inputs,
  cell,
}: let
  systemd-boot = import ./nixosProfiles/systemd-boot.nix;
in {
  inherit systemd-boot;
}
