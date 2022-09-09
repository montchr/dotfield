{
  withSystem,
  collective,
  self,
  ...
}: let
  inherit
    (self)
    inputs
    sharedModules
    sharedProfiles
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
    nvfetcher
    sops-nix
    ;
  inherit (inputs.digga.lib) flattenTree rakeLeaves;
  inherit (inputs.flake-utils.lib.system) aarch64-darwin x86_64-darwin;
  inherit (nixpkgs.lib) mapAttrs mapAttrs';
  inherit (self.darwinModules) hm-shared-config;

  inherit
    (import ../nixpkgs-config.nix {inherit self;})
    overlays
    packageOverrides
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

  specialArgs = {
    inherit darwinProfiles sharedModules sharedProfiles;
    roles = roles;
  };

  defaultModules =
    (builtins.attrValues (flattenTree darwinModules))
    ++ [
      {
        _module.args.self = self;
        _module.args.inputs = self.inputs;
        _module.args.primaryUser = primaryUser;
      }

      ({pkgs, ...}: {
        nix.nixPath = [
          "darwin=${darwin}"
          "nixpkgs=${pkgs.path}"
          "home-manager=${home-manager}"
        ];
        documentation.info.enable = false;
        nixpkgs = {
          inherit overlays;
          config = {
            inherit packageOverrides;
            allowUnfree = true;
          };
        };
      })

      hm-shared-config

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
  }:
    withSystem system (
      ctx @ {pkgs, ...}:
        makeOverridable (darwinSystem {
          inherit system specialArgs;
          modules =
            defaultModules
            ++ (builtins.attrValues sharedModules)
            ++ modules
            ++ [
              darwinMachines.${hostname}
            ];
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
