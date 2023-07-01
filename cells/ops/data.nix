# SPDX-FileCopyrightText: 2023 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
{
  inputs,
  cell,
}: let
  inherit (inputs.haumea.lib) load matchers;

  keys = load {
    src = ./data/keys;
    loader = [(matchers.always (_: builtins.readFile))];
  };

  metadata = load {
    src = ./data/metadata;
    inputs = {inherit keys;};
  };

  users = load {
    src = ./data/users;
    inputs = {inherit keys metadata;};
  };
in {inherit keys metadata users;}
