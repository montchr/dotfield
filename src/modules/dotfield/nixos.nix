# Copyright (C) 2024-2025 Chris Montgomery
# Copyright (C) 2025 Michael Belsanti
# SPDX-License-Identifier: GPL-2.0-or-later OR MIT

{
  lib,
  self,
  config,
  inputs,
  ...
}:
let
  inherit (builtins) filter map;

  lib' = self.lib;

  collectTypedModules = type: lib.foldr (v: acc: acc ++ v.${type}.imports) [ ];
  collectNixosModules = collectTypedModules "nixos";
  collectHomeModules = collectTypedModules "home";

  # Get all aspects that are active (direct + transitive) using the registry
  getAllActiveAspects =
    hostAspects: registry:
    let
      # Start with the aspects directly specified by the host
      directNames = map (a: a._name) hostAspects;

      # Recursively collect transitive imports from registry
      collectTransitive =
        aspectNames: visited:
        let
          newTransitives = lib.concatMap (name: registry.${name}.transitiveImports or [ ]) aspectNames;
          unvisited = lib.filter (name: !(lib.elem name visited)) newTransitives;
          newVisited = visited ++ unvisited;
        in
        if unvisited == [ ] then
          aspectNames
        else
          lib.unique (aspectNames ++ collectTransitive unvisited newVisited);
    in
    collectTransitive directNames directNames;

  overlays = (import (self.outPath + "/src/overlays/default.nix") { inherit inputs; });

  makeHost =
    hostName: hostConfig:
    let
      moduleArgs = {
        _module.args = {
          inherit lib';
        };
      };

      nixosModules =
        (collectNixosModules hostConfig.aspects)
        ++ hostConfig.nixos.imports
        ++ [
          config.dotfield.baseline.nixos
          moduleArgs
        ];

      homeModules = [
        config.dotfield.baseline.home
        hostConfig.baseline.home
        moduleArgs
      ];

      users = lib.mapAttrs (
        username: userConfig:
        let
          customAspects = config.dotfield.users.${username}.aspects;

          # Get all active aspects (direct + transitive) using the registry
          allActiveAspectNames = getAllActiveAspects userConfig.aspects config.dotfield.registry.aspects;

          # Get user aspects for all active aspect names
          mirroredAspects =
            allActiveAspectNames |> (map (name: customAspects.${name} or { })) |> filter (v: v != { });
        in
        {
          imports =
            homeModules
            ++ userConfig.home.imports
            ++ (collectHomeModules userConfig.aspects)
            ++ (collectHomeModules mirroredAspects)
            ++ (collectHomeModules config.dotfield.users.${username}.baseline.aspects)
            ++ [ (config.dotfield.users.${username}.baseline.home) ];
        }
      ) hostConfig.users;
    in
    inputs.nixpkgs.lib.nixosSystem {
      modules = nixosModules ++ [
        inputs.home-manager.nixosModules.default

        {
          networking = { inherit hostName; };
          nixpkgs.config.allowUnfree = true;
          nixpkgs = { inherit overlays; };
          home-manager.users = users;
        }
      ];
    };

in
{
  flake.nixosConfigurations = lib.mapAttrs makeHost config.dotfield.hosts.nixos;
}
