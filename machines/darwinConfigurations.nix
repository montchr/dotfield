{
  self,
  withSystem,
  ops,
  ...
}: let
  inherit (self) inputs lib;
  inherit (inputs) darwin haumea home-manager;

  darwinModules = import ../darwin/modules-list.nix;
  darwinProfiles = import ../profiles/darwinProfiles.nix {inherit haumea;};
  darwinSuites = import ../profiles/darwinSuites.nix {inherit sharedProfiles darwinProfiles;};
  sharedModules = import ../common/modules-list.nix;
  sharedProfiles = import ../profiles/sharedProfiles.nix {inherit haumea;};

  defaultModules = [
    sharedProfiles.core.default
    darwinProfiles.core.default
    home-manager.darwinModules.home-manager
  ];

  makeDarwinSystem = hostName: darwinArgs @ {system, ...}:
    withSystem system (
      {pkgs, ...}:
        darwin.lib.darwinSystem {
          inherit system;
          specialArgs = {
            inherit darwinProfiles sharedProfiles;
            flake = lib.modules.flakeSpecialArgs' system;
          };
          pkgs = darwinArgs.pkgs or pkgs;
          modules =
            defaultModules
            sharedModules
            ++ darwinModules
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
  # FIXME: re-expose with haumea?
  # flake.darwinModules = darwinModules';
  flake.darwinConfigurations = {
    tuvix = makeDarwinSystem "tuvix" {
      system = "aarch64-darwin";
      modules = [];
    };
  };
}
