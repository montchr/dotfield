{
  config,
  self,
  ...
}:
let
  inherit (self) inputs;
  inherit (self.lib.hm) makeHomeConfiguration;
  inherit (inputs.apparat.lib.hm) mkHomeConfigurations;
in
{
  flake = {
    # TODO: invert this approach -- make system configs import pre-defined home
    # configs or something like that. see ~misterio77/nixos-config
    # homeConfigurations = (mkHomeConfigurations config.flake.nixosConfigurations);
    homeConfigurations."cdom@tuuvok" = makeHomeConfiguration "cdom" {
      system = "aarch64-linux";
      modules = [ ../users/cdom/cdom-at-tuuvok.nix ];
    };
    homeModules = {
      "theme" = import ./modules/theme/default.nix;
      "programs/bash/trampoline" = import ./modules/programs/bash/trampoline.nix;
      "programs/cod" = import ./modules/programs/cod.nix;
      "programs/liquidprompt" = import ./modules/programs/liquidprompt.nix;
    };
  };

  perSystem = _perSystem: {
    homeConfigurations = {
      traveller = makeHomeConfiguration "cdom" {
        modules = [
          ./profiles/development/work/default.nix
        ];
      };
    };
  };
}
