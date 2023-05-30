{
  self,
  withSystem,
  ops,
  ...
}: let
  inherit (self) inputs lib;
  inherit (inputs) apparat darwin haumea;
  inherit (apparat.lib) flattenTree;
  l = inputs.nixpkgs.lib // builtins;

  darwinModules = import ../modules/darwinModules.nix;
  darwinProfiles = import ../profiles/darwinProfiles.nix {inherit haumea;};
  darwinSuites = import ../profiles/darwinSuites.nix {inherit sharedProfiles darwinProfiles;};
  sharedModules = import ../modules/sharedModules.nix {inherit haumea;};
  sharedProfiles = import ../profiles/sharedProfiles.nix {inherit haumea;};

  darwinModules' = flattenTree darwinModules;
  sharedModules' = flattenTree sharedModules;

  defaultModules = [
    sharedProfiles.core.default
    darwinProfiles.core.default
    inputs.agenix.nixosModules.age
    inputs.home-manager.darwinModules.home-manager
    # TODO: darwin support yet?
    # inputs.sops-nix.darwinModules.sops
  ];

  makeDarwinSystem = hostName: darwinArgs @ {system, ...}:
    withSystem system (
      {pkgs, ...}:
        l.makeOverridable darwin.lib.darwinSystem {
          inherit system;
          specialArgs = {
            inherit darwinProfiles sharedProfiles;
            flake = lib.modules.flakeSpecialArgs' system;
          };
          pkgs = darwinArgs.pkgs or pkgs;
          modules =
            defaultModules
            ++ (l.attrValues sharedModules')
            ++ (l.attrValues darwinModules')
            ++ (darwinArgs.modules or [])
            ++ darwinSuites.workstation
            ++ [
              ./${hostName}
              {
                _module.args = {inherit ops;};
                networking.hostName = hostName;
                networking.computerName = hostName;
              }
            ];
        }
    );
in {
  flake.darwinModules = darwinModules';
  flake.darwinConfigurations = {
    tuvix = makeDarwinSystem "tuvix" {
      system = "aarch64-darwin";
      modules = [];
    };
  };
}
