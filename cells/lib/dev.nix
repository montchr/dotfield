# SPDX-FileCopyrightText: 2022-2023 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
# FIXME: delete -- use apparat
{
  inputs,
  cell,
}: rec {
  pkgWithCategory = category: package: withCategory category {inherit package;};
  withCategory = category: attrs: attrs // {inherit category;};
}
