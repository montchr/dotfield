{haumea}: let
  homeProfiles = haumea.lib.load {
    src = ./homeProfiles;
    loader = haumea.lib.loaders.path;
  };
in
  homeProfiles
