# SPDX-FileCopyrightText: 2023 Chris Montgomery <chmont@proton.me>
# SPDX-License-Identifier: GPL-3.0-or-later
{ inputs, ... }:
let
  inherit (inputs.haumea.lib) load loaders matchers;
in
{
  dotfield.meta.keys = load {
    src = ./data;
    loader = [
      (matchers.nix loaders.default)
      (matchers.always (_: builtins.readFile))
    ];
  };
}
