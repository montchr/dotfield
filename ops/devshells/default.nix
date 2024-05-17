# SPDX-FileCopyrightText: 2022-2023 Chris Montgomery <chmont@proton.me>
# SPDX-License-Identifier: GPL-3.0-or-later
{
  imports = [
    ./dotfield.nix
    ./ci.nix
    ./secrets.nix
  ];

  perSystem = _: {
    # devshells.default = config.devshells.dotfield;
  };
}
