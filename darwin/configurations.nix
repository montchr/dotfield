{
  self,
  lib,
  withSystem,
  # DEPRECATED:
  peers,
  primaryUser,
  ...
}: let
  inherit
    (self)
    inputs
    sharedModules
    sharedProfiles
    ;
  inherit (self.nixosModules) homeManagerSettings;
  inherit
    (inputs.digga.lib)
    flattenTree
    rakeLeaves
    ;
  inherit (inputs.flake-utils.lib.system) aarch64-darwin;
  inherit (inputs.darwin.lib) darwinSystem;

  l = lib // builtins;

  roles = import ./roles.nix {inherit sharedProfiles darwinProfiles;};

  darwinModules = rakeLeaves ./modules;
  darwinMachines = rakeLeaves ./machines;
  darwinProfiles = rakeLeaves ./profiles;

  defaultModules = [
    sharedProfiles.core
    homeManagerSettings
    darwinProfiles.core
    inputs.home-manager.darwinModules.home-manager
    # `nixosModules` is correct, even for darwin
    inputs.agenix.nixosModules.age
    # inputs.sops-nix.darwinModules.sops
  ];

  makeDarwinSystem = hostName: args:
    withSystem (args.system or aarch64-darwin) (
      ctx @ {
        system,
        pkgs,
        inputs',
        ...
      }: let
        moduleArgs = {
          _module.args = {
            inherit inputs' peers primaryUser;
            inherit (ctx.config) packages;
            isNixos = false;
          };
        };
      in
        l.makeOverridable darwinSystem {
          inherit system;
          pkgs = args.pkgs or pkgs;
          modules =
            defaultModules
            ++ (builtins.attrValues sharedModules)
            ++ (builtins.attrValues (flattenTree darwinModules))
            ++ (args.modules or [])
            ++ roles.workstation
            ++ [
              moduleArgs
              {
                networking.hostName = hostName;
                networking.computerName = hostName;
                home-manager.sharedModules = [{_module.args.isNixos = false;}];
              }
              darwinMachines.${hostName}
            ];
          specialArgs = {
            inherit
              self
              inputs
              darwinProfiles
              sharedProfiles
              roles
              system
              ;
            inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux isMacOS;
          };
        }
    );
in {
  # flake.darwinModules = importLeaves darwinModules;
  # flake.darwinProfiles = importLeaves darwinProfiles;

  flake.darwinConfigurations = {
    tuvix = makeDarwinSystem "tuvix" {
      modules = [
        darwinProfiles.virtualisation.nixos-vm-host
      ];
    };
  };
}
