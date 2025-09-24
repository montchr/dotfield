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
          userAspects = config.users.${username}.aspects;
          resolvedUserAspects = (
            userSpec.aspects
            ++ userAspectDeps
            ++ userExtendedAspects
            ++ userExtendedAspectsDeps
            ++ [ (userAspects.core or { }) ]
          );
          userAspectDeps =
            (collectRequires config.aspects userSpec.aspects) ++ (collectRequires userAspects userSpec.aspects);
          userExtendedAspects = collectNameMatches (
            hostAspects ++ userSpec.aspects ++ userAspectDeps
          ) userAspects;
          # BUG: There is no point to further collecting deps from
          # *other* user aspects here, because those should just be
          # defined directly in the extended aspect definition.
          # However, the logic of this enforced limitation may or may
          # not hold up, because in truth it is to avoid an error
          # encountered during the refactoring of the kanata modules.
          # Be warned that attempting to source dependencies from more
          # than one aspect group will sooner or later lead to an error
          # where one of the aspect groups is missing the dependency.
          # Because `collectRequires` currently does not handle more
          # than one aspect group as its first argument, an
          # attribute-not-found error can be thrown even if the desired
          # aspect exists in the other group.  This is probably a bug,
          # but it is not currently worth fixing.  That said, it could
          # affect any usage of `collectRequires`, including the usage
          # in the definition of `userAspectDeps` above.
          userExtendedAspectsDeps = (collectRequires config.aspects userExtendedAspects);
        in
        {
          imports =
            homeModules
            ++ (collectHomeModules hostAspects)
            ++ (collectHomeModules resolvedUserAspects)
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
          nixpkgs.overlays = [ self.overlays.default ];
          home-manager.users = lib.mapAttrs makeHome hostSpec.users;
        }
      ];
    };
in
{
  flake.nixosConfigurations = lib.mapAttrs makeHost config.hosts.nixos;
}
