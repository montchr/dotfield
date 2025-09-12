{
  config,
  lib,
  inputs,
  self,
  ...
}:
let
  inherit (lib)
    filter
    map
    ;

  inherit (self.lib.modules)
    collectNixosModules
    collectHomeModules
    collectNameMatches
    collectRequires
    ;

  overlays = (import (self.outPath + "/overlays/default.nix") { inherit inputs; });

  makeHost =
    hostName: hostSpec:
    let
      inherit (hostSpec) system;

      hostAspectDeps = collectRequires config.aspects hostSpec.aspects;
      hostAspects = hostSpec.aspects ++ hostAspectDeps;

      nixosModules = (collectNixosModules hostAspects) ++ [
        inputs.home-manager.nixosModules.default
        inputs.sops-nix.nixosModules.sops
        inputs.stylix.nixosModules.stylix
        config.aspects.core.nixos
        hostSpec.configuration
      ];

      homeModules = [
        config.aspects.core.home
        hostSpec.baseline.home
      ];

      makeHome =
        username: userSpec:
        let
          userAspects =
            userSpec.aspects
            ++ userAspectDeps
            ++ userExtendedAspects
            ++ [
              (config.users.${username}.aspects.core or { })
            ];
          userAspectDeps =
            (collectRequires config.aspects userSpec.aspects)
            ++ (collectRequires config.users.${username}.aspects userSpec.aspects);
          userExtendedAspects = collectNameMatches (
            hostAspects ++ userSpec.aspects ++ userAspectDeps
          ) config.users.${username}.aspects;
          # parentAspects = collectNameMatches userAspects config.aspects;
        in

        {
          imports =
            homeModules
            ++ (collectHomeModules hostAspects)
            ++ (collectHomeModules userAspects)
            #  ++ (collectHomeModules parentAspects)
            ++ (collectHomeModules config.users.${username}.baseline.aspects)
            ++ [
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
