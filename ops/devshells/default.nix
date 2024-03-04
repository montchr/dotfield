# SPDX-FileCopyrightText: 2022-2023 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
{
  imports = [
    ./dotfield.nix
    ./ci.nix
    ./secrets.nix
  ];

  perSystem = {config, ...}: {
    # devshells.default = config.devshells.dotfield;
  };
}
