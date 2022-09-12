{
  withSystem,
  collective,
  self,
  ...
}: let
  inherit
    (self)
    inputs
    nixpkgsConfig
    sharedModules
    sharedProfiles
    ;
  inherit
    (self.darwinModules)
    hm-shared-config
    ;
  inherit
    (inputs)
    nixpkgs
    nixos-stable
    nixos-unstable
    darwin
    agenix
    home-manager
    nur
    sops-nix
    ;
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
    (nixpkgs.lib)
    makeOverridable
    mapAttrs
    mapAttrs'
    ;
  inherit
    (darwin.lib)
    darwinSystem
    ;

  roles = import ./roles {inherit sharedProfiles darwinProfiles;};

  # FIXME: move to guardian
  primaryUser.authorizedKeys = import ../secrets/authorized-keys.nix;

  darwinModules = rakeLeaves ./modules;
  darwinMachines = rakeLeaves ./machines;
  darwinProfiles = rakeLeaves ./profiles;

  defaultModules =
    (builtins.attrValues (flattenTree darwinModules))
    ++ [
      ({pkgs, ...}: {
        imports = [nixpkgsConfig];
        nix.nixPath = [
          "darwin=${darwin}"
          "nixpkgs=${pkgs.path}"
          "home-manager=${home-manager}"
        ];
        documentation.info.enable = false;
      })

      sharedProfiles.core
      sharedProfiles.homeManagerSettings

      darwinProfiles.core
      home-manager.darwinModules.home-manager
      # `nixosModules` is correct, even for darwin
      # FIXME: migrate to sops
      agenix.nixosModules.age
    ];

  makeDarwinSystem = hostname: {
    system ? aarch64-darwin,
    modules ? [],
  }:
    withSystem system (
      ctx @ {sources, ...}: let
        moduleArgs = {
          _module.args.self = self;
          _module.args.inputs = self.inputs;
          _module.args.inputs' = self.inputs';
          _module.args.primaryUser = primaryUser;
          _module.args.packages = ctx.config.packages;
          _module.args.sources = sources;
        };
      in
        makeOverridable (darwinSystem {
          inherit system;
          modules =
            defaultModules
            ++ (builtins.attrValues sharedModules)
            ++ modules
            ++ [
              moduleArgs
              {home-manager.sharedModules = [moduleArgs];}
              darwinMachines.${hostname}
            ];
          specialArgs = {
            inherit
              darwinProfiles
              sharedProfiles
              roles
              ;
          };
        })
    );
in {
  # flake.darwinModules = importLeaves darwinModules;
  # flake.darwinProfiles = importLeaves darwinProfiles;
  flake.darwinConfigurations = {
    cdotmp = makeDarwinSystem "cdotmp" {
      system = x86_64-darwin;
    };
  };
}
