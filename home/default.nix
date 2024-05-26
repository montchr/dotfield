{
  config,
  self,
  ops,
  ...
}:
let
  inherit (self) inputs;
  inherit (self.lib.hm) makeHomeConfiguration;

  # features = import ./features.nix { homeProfiles = profiles; };
  profiles = import ./profiles.nix { inherit (inputs) haumea; };
in
{
  flake = {

    homeModules = {
      "theme" = import ./modules/theme/default.nix;
      "whoami" = import ./modules/dotfield/whoami.nix;
      "programs/bash/trampoline" = import ./modules/programs/bash/trampoline.nix;
      "programs/cod" = import ./modules/programs/cod.nix;
      "programs/liquidprompt" = import ./modules/programs/liquidprompt.nix;
    };
  };
}
