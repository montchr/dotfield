{
  withSystem,
  collective,
  self,
  lib,
  sharedModules,
  sharedProfiles,
  ...
}: let
  inherit (self) inputs;
  inherit (lib) importLeaves;
  inherit (inputs) agenix home-manager digga;
  inherit (inputs.digga.lib) flattenTree rakeLeaves;
  inherit (inputs.flake-utils.lib.system) aarch64-darwin x86_64-darwin;
  inherit (inputs.nix-darwin.lib) darwinSystem;

  # FIXME: move to guardian
  primaryUser.authorizedKeys = import ../secrets/authorized-keys.nix;

  darwinMachines = rakeLeaves ./machines;
  darwinModules = rakeLeaves ./modules;
  darwinProfiles = rakeLeaves ./profiles;

  roles = import ./roles {inherit sharedProfiles darwinProfiles;};

  defaultModules = [
    (builtins.attrValues self.darwinModules)
    {
      _module.args = {
        inherit
          self
          inputs
          collective
          darwinProfiles
          primaryUser
          ;
      };
    }
    sharedProfiles.core
    darwinProfiles.core
    home-manager.darwinModules.home-manager
    # `nixosModules` is correct, even for darwin
    # FIXME: migrate to sops
    agenix.nixosModules.age
  ];

  makeDarwinSystem = hostname: {
    system ? aarch64-darwin,
    modules ? [],
    extraModuleArgs ? {},
  }:
    withSystem system (ctx @ {pkgs, ...}:
      pkgs.lib.makeOverridable (darwinSystem {
        inherit inputs system;
        modules =
          defaultModules
          ++ sharedModules
          ++ modules
          ++ [
            ({_module.args.packages = ctx.config.packages;} // extraModuleArgs)
            darwinMachines.${hostname}
          ];
      }));
in {
  flake.darwinModules = importLeaves darwinModules;
  flake.darwinProfiles = importLeaves darwinProfiles;
  flake.darwinConfigurations = {
    cdotmp = makeDarwinSystem "cdotmp" {
      system = x86_64-darwin;
    };
  };
}
