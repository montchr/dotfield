# SPDX-FileCopyrightText: 2023 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
{inputs}: {
  data = {};
  output = ".sops.yaml";
  format = "yaml";
  packages = [inputs.nixpkgs.sops];
}
