# SPDX-FileCopyrightText: 2022-2023 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
{inputs}: {
  output = "statix.toml";
  format = "toml";
  data = {};
  packages = [inputs.nixpkgs.statix];
}
