{
  self,
  withSystem,
  ops,
  ...
}:
let
  inherit (self) inputs lib;
  inherit (inputs) darwin haumea home-manager;

  sharedModules = import ../common/modules-list.nix;
  sharedProfiles = import ../common/profiles.nix { inherit haumea; };

  darwinModules = import ./modules-list.nix;
  profiles = import ./profiles.nix { inherit haumea; };

  features = import ./features.nix { inherit sharedProfiles profiles; };

  defaultModules = [
    sharedProfiles.core.default
    profiles.core.default
    home-manager.darwinModules.home-manager
  ];

  makeDarwinSystem =
    hostName:
    darwinArgs@{ system, ... }:
    withSystem system (
      { pkgs, ... }:
      darwin.lib.darwinSystem {
        inherit system;
        specialArgs = {
          inherit sharedProfiles profiles;
          flake = lib.modules.flakeSpecialArgs' system;
        };
        pkgs = darwinArgs.pkgs or pkgs;
        modules =
          defaultModules sharedModules
          ++ darwinModules
          ++ (darwinArgs.modules or [ ])
          ++ features.workstation
          ++ [
            ./${hostName}
            {
              _module.args = {
                inherit ops;
              };
              networking.hostName = hostName;
              networking.computerName = hostName;
            }
          ];
      }
    );
in
{
  # FIXME: re-expose with haumea?
  # flake.darwinModules = darwinModules';
  flake.darwinConfigurations = {
    tuvix = makeDarwinSystem "tuvix" {
      system = "aarch64-darwin";
      modules = [ ];
    };
  };
}
