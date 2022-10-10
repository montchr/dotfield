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

  # FIXME: move to guardian
  primaryUser.authorizedKeys = import ../secrets/authorized-keys.nix;

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
            # FIXME: this may have been necessary at somepoint, but is it still?
            config.allowBroken = true;
          });
        moduleArgs = {
          _module.args.self = self;
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
            ++ (args.modules or [])
            # It's extremely unlikely that a Darwin system will ever be
            # anything other than a "workstation" i.e. a laptop running macOS.
            # Until that changes, there's no need to import this role in every
            # Darwin host.
            ++ roles.workstation
            ++ [
              moduleArgs
              {
                # FIXME: nix-darwin upstream does not support these options, which exist on nixos
                # nixpkgs.pkgs = mkDefault pkgs;
                # nixpkgs.localSystem = mkDefault pkgs.stdenv.hostPlatform;
                networking.hostName = hostname;
                home-manager.sharedModules = [moduleArgs];
              }
              darwinMachines.${hostname}
            ];
          specialArgs = {
            inherit
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
    tuvix = makeDarwinSystem "tuvix" {};
    cdotmp = makeDarwinSystem "cdotmp" {
      system = x86_64-darwin;
    };
  };
}
