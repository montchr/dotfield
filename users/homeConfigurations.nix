{
  config,
  self,
  ops,
  ...
}: let
  inherit (self) inputs;
  inherit (self.lib.hm) makeHomeConfiguration;
  inherit (inputs.apparat.lib.hm) mkHomeConfigurations;

  homeProfiles = import ../profiles/homeProfiles.nix {inherit (inputs) haumea;};
  homeSuites = import ../profiles/homeSuites.nix {inherit homeProfiles;};
in {
  flake = {
    # FIXME: re-expose with haumea?
    # inherit homeModules;
    homeConfigurations =
      (mkHomeConfigurations config.flake.nixosConfigurations)
      // (mkHomeConfigurations config.flake.darwinConfigurations);
  };

  perSystem = _perSystem: {
    homeConfigurations = {
      traveller = makeHomeConfiguration "cdom" {
        modules =
          homeSuites.webdev
          ++ [{_module.args = {inherit ops;};}];
      };
    };
  };
}
