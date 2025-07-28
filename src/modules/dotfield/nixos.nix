# Copyright (C) 2024-2025 Chris Montgomery
# Copyright (C) 2025 Michael Belsanti
# SPDX-License-Identifier: GPL-2.0-or-later OR MIT

{
  self,
  lib,
  config,
  inputs,
  ...
}:
let

  inherit (lib) mkOption types;
  inherit (lib'.modules) mkDeferredModuleOption scopedModulesOptions scopedSubmoduleType;
  haumea = inputs.haumea.lib;
  lib' = self.lib;

  # Load user modules directly here (not through module args to avoid circular deps)
  userModules = haumea.load {
    src = "${self.outPath}/src/users";
    loader = haumea.loaders.verbatim;
  };

  collectNixosModules = builtins.foldl' (v: acc: acc ++ v.nixos.imports) [ ];
  collectHomeModules = builtins.foldl' (v: acc: acc ++ v.home.imports) [ ];

  # Collect user-specific home modules for a given username
  # FIXME: needs update
  collectUserHomeModules =
    username: requestedModuleNames:
    let
      userFeatures = userModules.${username}.features or { };
      userHomeModules = lib.mapAttrsToList (
        featureName: featureModule:
        if builtins.elem featureName requestedModuleNames && featureModule ? home then
          featureModule.home
        else
          null
      ) userFeatures;
    in
    builtins.filter (m: m != null) userHomeModules;

in
{
  options.dotfield.hosts.nixos = mkOption {
    type = types.attrsOf (
      types.submodule (
        { name, ... }:
        let
          scopedModulesListOption = mkOption {
            type = types.listOf scopedSubmoduleType;
            default = [ ];
          };
        in
        {
          options = {
            nixos = mkDeferredModuleOption "Host-specific NixOS configuration";
            home = mkDeferredModuleOption "Host-specific home-manager configuration, applied to all users for host.";
            modules = scopedModulesListOption;
            name = mkOption {
              default = name;
              readOnly = true;
              description = "Hostname";
            };
            users = mkOption {
              type = types.lazyAttrsOf (
                types.submodule {
                  options = {
                    modules = scopedModulesListOption;
                    home = mkDeferredModuleOption "User-and-host-specific home-manager configuration";
                  };
                }
              );
              default = { };
            };
          };
        }
      )
    );
  };

  config = {
    flake.nixosConfigurations =
      config.dotfield.hosts.nixos
      |> lib.mapAttrs (
        hostName: hostConfig:
        let
          moduleArgs = {
            _module.args = {
              inherit lib';
            };
          };

          nixosModules =
            (collectNixosModules hostConfig.modules)
            ++ hostConfig.nixos.imports
            ++ [
              config.dotfield.nixos
              moduleArgs
            ];

          homeModules = [
            config.dotfield.home
            hostConfig.home
            moduleArgs
          ];

          # FIXED: User-specific modules loaded from separate import tree
          # Global modules come from src/features, user modules from src/users/<username>/features
          users = lib.mapAttrs (
            username: userConfig:
            let
              globalHomeModules = collectHomeModules userConfig.modules;
              # Extract module names for user module lookup
              moduleNames = map (
                m: if builtins.isString m then m else m.name or (throw "Cannot determine module name")
              ) userConfig.modules;
              userHomeModules = collectUserHomeModules username moduleNames;
            in
            {
              imports = globalHomeModules ++ userHomeModules ++ userConfig.home.imports ++ homeModules;
            }
          ) hostConfig.users;

        in
        inputs.nixpkgs.lib.nixosSystem {
          modules = nixosModules ++ [
            inputs.home-manager.nixosModules.default
            {
              networking = { inherit hostName; };
              nixpkgs.config.allowUnfree = true;
              nixpkgs.overlays = (import "${self.outPath}/overlays/default.nix" { inherit inputs; });
              home-manager.users = users;
            }
          ];
        }
      );
  };
}
