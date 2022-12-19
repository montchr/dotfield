# SPDX-FileCopyrightText: 2022 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
{
  inputs,
  cell,
}: rec {
  pkgWithCategory = category: package: withCategory category {inherit package;};
  withCategory = category: attrs: attrs // {inherit category;};
}
