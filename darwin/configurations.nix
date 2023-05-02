{
  self,
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
  inherit (inputs.apparat.lib) tree;
  inherit (inputs.flake-utils.lib.system) aarch64-darwin;
  l = inputs.nixpkgs.lib // builtins;

  roles = import ./roles.nix {inherit sharedProfiles darwinProfiles;};

  darwinModules = tree.rake ./modules;
  darwinMachines = tree.rake ./machines;
  darwinProfiles = tree.rake ./profiles;

  defaultModules = [
    (sharedProfiles."core/default")
    homeManagerSettings
    darwinProfiles.core
    inputs.home-manager.darwinModules.home-manager
    inputs.agenix.nixosModules.age
    # inputs.sops-nix.darwinModules.sops
  ];

  makeDarwinSystem = hostName: darwinArgs @ {system, ...}:
    withSystem system (
      ctx @ {
        inputs',
        packages,
        cells,
        pkgs,
        ...
      }:
        l.makeOverridable inputs.darwin.lib.darwinSystem {
          inherit system;
          pkgs = darwinArgs.pkgs or pkgs;
          modules =
            defaultModules
            ++ (l.attrValues sharedModules)
            ++ (l.attrValues (tree.flatten darwinModules))
            ++ (darwinArgs.modules or [])
            ++ roles.workstation
            ++ [
              {
                _module.args = {
                  inherit inputs' peers primaryUser;
                  inherit cells;
                  inherit (ctx.config) packages;
                  isNixos = false;
                };
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
              packages
              roles
              sharedProfiles
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
      system = aarch64-darwin;
      modules = [];
    };
  };
}
