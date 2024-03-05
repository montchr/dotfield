# SPDX-FileCopyrightText: 2023 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
{self, ...}: let
  inherit (self.inputs.haumea.lib) load matchers;

  keys = load {
    src = ./keys;
    loader = [(matchers.always (_: builtins.readFile))];
  };

  metadata = load {
    src = ./metadata;
    inputs = {inherit keys;};
  };

  users = load {
    src = ./users;
    inputs = {inherit keys metadata;};
  };
in {inherit keys metadata users;}
