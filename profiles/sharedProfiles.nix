{haumea}: let
  sharedProfiles = haumea.lib.load {
    src = ./sharedProfiles;
    loader = haumea.lib.loaders.path;
  };
in
  sharedProfiles
