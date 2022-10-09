{
  withSystem,
  self,
  peers,
  lib,
  ...
}: let
  inherit
    (self)
    inputs
    sharedModules
    sharedProfiles
    ;
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
    mapAttrs
    mapAttrs'
    optionalAttrs
    ;
  inherit (inputs.darwin.lib) darwinSystem;
  inherit (lib) mkBefore;

  roles = import ./roles {inherit sharedProfiles darwinProfiles;};

  # FIXME: move to guardian
  primaryUser.authorizedKeys = import ../secrets/authorized-keys.nix;

  darwinModules = rakeLeaves ./modules;
  darwinMachines = rakeLeaves ./machines;
  darwinProfiles = rakeLeaves ./profiles;

  defaultModules = [
    sharedProfiles.core
    sharedProfiles.homeManagerSettings
    darwinProfiles.core
    inputs.home-manager.darwinModules.home-manager
    # `nixosModules` is correct, even for darwin
    inputs.agenix.nixosModules.age
  ];

  makeDarwinSystem = hostname: {
    system ? aarch64-darwin,
    pkgs ? (withSystem system (ctx @ {...}: ctx.pkgs)),
    modules ? [],
  }:
    withSystem system (
      ctx @ {...}: let
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
            config.allowBroken = true;
          });
        moduleArgs = {
          _module.args.inputs = self.inputs;
          _module.args.primaryUser = primaryUser;
          _module.args.packages = ctx.config.packages;
          _module.args.sources = ctx.sources;
          _module.args.peers = peers;
        };
      in
        darwinSystem {
          inherit pkgs system;
          modules =
            defaultModules
            ++ (builtins.attrValues sharedModules)
            ++ (builtins.attrValues (flattenTree darwinModules))
            ++ modules
            # It's extremely unlikely that a Darwin system will ever be
            # anything other than a "workstation" i.e. a laptop running macOS.
            # Until that changes, there's no need to import this role in every
            # Darwin host.
            ++ roles.workstation
            ++ [
              moduleArgs
              {
                networking.hostName = hostname;
                home-manager.sharedModules = [moduleArgs];
              }
              darwinMachines.${hostname}
            ];
          specialArgs = {
            inherit
              self
              inputs
              darwinProfiles
              sharedProfiles
              roles
              rosettaPkgs
              ;
          };
        }
    );
in {
  # flake.darwinModules = importLeaves darwinModules;
  # flake.darwinProfiles = importLeaves darwinProfiles;

  flake.darwinConfigurations = {
    tuvix = makeDarwinSystem "tuvix" {};
    cdotmp = makeDarwinSystem "cdotmp" {
      system = x86_64-darwin;
    };
  };
}
