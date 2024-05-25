{
  config,
  self,
  ops,
  ...
}:
let
  inherit (self) inputs;
  inherit (self.lib.hm) makeHomeConfiguration;
  inherit (inputs.apparat.lib.hm) mkHomeConfigurations;

  # features = import ./features.nix { homeProfiles = profiles; };
  profiles = import ./profiles.nix { inherit (inputs) haumea; };
in
{
  flake = {
    # TODO: invert this approach -- make system configs import pre-defined home
    # configs or something like that. see ~misterio77/nixos-config
    homeConfigurations = (mkHomeConfigurations config.flake.nixosConfigurations);

    homeModules = {
      "theme" = import ./modules/theme/default.nix;
      "whoami" = import ./modules/dotfield/whoami.nix;
      "programs/bash/trampoline" = import ./modules/programs/bash/trampoline.nix;
      "programs/cod" = import ./modules/programs/cod.nix;
      "programs/liquidprompt" = import ./modules/programs/liquidprompt.nix;
    };
  };

  perSystem = _perSystem: {
    homeConfigurations = {
      traveller = makeHomeConfiguration "cdom" {
        modules = [
          profiles.development.work

          {
            _module.args = {
              inherit ops;
            };
          }
        ];
      };
    };
  };
}
