{haumea}: let
  sharedModules = haumea.lib.load {
    src = ./sharedModules;
    loader = haumea.lib.loaders.path;
  };
in
  sharedModules
