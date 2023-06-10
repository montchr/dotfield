{haumea}: let
  nixosProfiles = haumea.lib.load {
    src = ./nixosProfiles;
    loader = haumea.lib.loaders.path;
  };
in
  nixosProfiles
