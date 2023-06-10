{haumea}: let
  inherit (haumea.lib) load loaders matchers;
  keys = load {
    src = ./keys;
    loader = [(matchers.always (_: builtins.readFile))];
  };
  metadata = load {
    src = ./metadata;
    loader = loaders.verbatim;
  };
  users = load {
    src = ./users;
    inputs = {inherit keys metadata;};
  };
in {inherit keys metadata users;}
