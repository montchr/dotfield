# SPDX-FileCopyrightText: 2022 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
{
  inputs,
  cell,
}: {
  output = ".lintstagedrc.json";
  format = "json";
  configData = {};
  # packages = [cell.packages.lint-staged];
}
