{
  config,
  lib,
  inputs,
  self,
  ...
}:
let
  inherit (lib) filter map;

  nixos = self.outPath + "/nixos";
  home = self.outPath + "/home";

  collectTypedModules = type: lib.foldr (v: acc: acc ++ v.${type}.imports) [ ];
  collectNixosModules = collectTypedModules "nixos";
  collectHomeModules = collectTypedModules "home";

  overlays = (import (self.outPath + "/overlays/default.nix") { inherit inputs; });

  makeHost =
    hostName: hostSpec:
    let
      inherit (hostSpec) system;

      profiles = nixos + "/profiles";

      nixosModules =
        (collectNixosModules hostSpec.aspects)
        ++ (import (nixos + "/modules-list.nix"))
        ++ [
          #          inputs.home-manager.nixosModules.default
          inputs.sops-nix.nixosModules.sops

          config.aspects.core.nixos

          hostSpec.configuration
        ];

      homeModules = (import (home + "/modules-list.nix")) ++ [
        config.aspects.core.home
        hostSpec.baseline.home
      ];

      makeHome =
        username: userSpec:
        let
          userAspects = config.users.${username}.aspects;

          # Collect user-customized aspects sharing the same name as the
          # specified host aspects and host-user aspects.
          extendedAspects =
            (hostSpec.aspects ++ userSpec.aspects)
            |> (map (v: userAspects.${v._name} or null))
            |> filter (v: v != null);

          # Collect global aspects sharing the same name as the
          # specified host-user aspects.
          parentAspects =
            userSpec.aspects |> (map (v: config.aspects.${v._name} or null)) |> filter (v: v != null);
        in
        {
          imports =
            homeModules
            ++ (collectHomeModules hostSpec.aspects)
            ++ (collectHomeModules userSpec.aspects)
            ++ (collectHomeModules extendedAspects)
            ++ (collectHomeModules parentAspects)
            ++ (collectHomeModules config.users.${username}.baseline.aspects)
            ++ [
              config.users.${username}.baseline.configuration
              userSpec.configuration
            ];
        };
    in
    inputs.${hostSpec.channel}.lib.nixosSystem {
      inherit system;
      modules = nixosModules ++ [
        {
          networking = { inherit hostName; };
          nixpkgs.config.allowUnfree = true;
          nixpkgs = { inherit overlays; };
          home-manager.users = lib.mapAttrs makeHome hostSpec.users;
        }
      ];
      specialArgs = {
        flake = self.lib.modules.flakeSpecialArgs' system;
      };
    };
in
{
  flake.nixosConfigurations = lib.mapAttrs makeHost config.hosts.nixos;
}
