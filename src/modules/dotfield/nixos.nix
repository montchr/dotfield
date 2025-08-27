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
          mirroredAspects =
            userConfig.aspects |> (map (v: customAspects.${v._name} or { })) |> filter (v: v != { });
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
    inputs.${hostConfig.channel}.lib.nixosSystem {
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
