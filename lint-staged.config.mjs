// SPDX-FileCopyrightText: 2022 Chris Montgomery <chris@cdom.io>
// SPDX-License-Identifier: GPL-3.0-or-later

export default {
  '*': 'nix run nixpkgs#treefmt -- --no-cache',
  '*.nix': 'just lint-nix',
};
