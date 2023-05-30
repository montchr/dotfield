{haumea}: let
  homeModules = haumea.lib.load {
    src = ./homeModules;
    loader = haumea.lib.loaders.path;
  };
in
  homeModules
