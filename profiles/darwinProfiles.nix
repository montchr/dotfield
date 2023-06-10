{haumea}: let
  darwinProfiles = haumea.lib.load {
    src = ./darwinProfiles;
    loader = haumea.lib.loaders.path;
  };
in
  darwinProfiles
