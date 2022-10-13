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
  inherit (self.inputs) nixpkgs;
  inherit
    (inputs.digga.lib)
    flattenTree
    rakeLeaves
    ;
  inherit
    (inputs.flake-utils.lib.system)
    aarch64-darwin
    x86_64-darwin
    ;
  inherit
    (lib)
    makeOverridable
    optionalAttrs
    ;
  inherit (inputs.darwin.lib) darwinSystem;

  roles = import ./roles {inherit sharedProfiles darwinProfiles;};

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

  makeDarwinSystem = hostname: args:
    withSystem (args.system or aarch64-darwin) (
      ctx @ {system, ...}: let
        pkgs = args.pkgs or ctx.pkgs;

        # Cross-compiled package set via Rosetta for packages which fail to
        # build on `aarch64-darwin`.
        #
        # NOTE: I have not yet had to use this, fortunately, but that means it's
        # untested.
        rosettaPkgs =
          optionalAttrs (system == aarch64-darwin)
          (import nixpkgs {
            system = x86_64-darwin;
            config.allowUnfree = true;
          });
        moduleArgs = {
          _module.args = {
            inherit peers primaryUser rosettaPkgs;
            inherit (ctx.config) packages;
          };
        };
      in
        makeOverridable darwinSystem {
          inherit pkgs system;
          modules =
            defaultModules
            ++ (builtins.attrValues sharedModules)
            ++ (builtins.attrValues (flattenTree darwinModules))
            ++ (args.modules or [])
            # It's extremely unlikely that a Darwin system will ever be
            # anything other than a "workstation" i.e. a laptop running macOS.
            # Until that changes, there's no need to import this role in every
            # Darwin host.
            ++ roles.workstation
            ++ [
              moduleArgs
              {networking.hostName = hostname;}
              darwinMachines.${hostname}
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
