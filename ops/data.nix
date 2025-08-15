# SPDX-FileCopyrightText: 2023 Chris Montgomery <chmont@proton.me>
# SPDX-License-Identifier: GPL-3.0-or-later
{ haumea }:
let
  inherit (haumea.lib) load loaders matchers;
in
load {
  src = ./data;
  loader = [
    (matchers.nix loaders.default)
    (matchers.always (_: builtins.readFile))
  ];
}
