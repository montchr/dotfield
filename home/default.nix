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
    # FIXME: invert this approach -- make system configs import pre-defined home
    # configs or something like that. see ~misterio77/nixos-config
    homeConfigurations =
      (mkHomeConfigurations config.flake.nixosConfigurations)
      // (mkHomeConfigurations config.flake.darwinConfigurations);
    # FIXME: re-expose with haumea?
    # inherit homeModules;
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
