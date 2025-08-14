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
  inherit (lib'.modules) mkDeferredModuleOpt mkFeatureListOpt;
  lib' = self.lib;

  collectNixosModules = builtins.foldl' (v: acc: acc ++ v.nixos.imports) [ ];
  collectHomeModules = builtins.foldl' (v: acc: acc ++ v.home.imports) [ ];
  collectUserFeatures = username: (lib'.fs.tree (self.outPath + src/users/${username}/features));

  hostSubmodule = (
    { name, ... }:
    {
      options = {
        nixos = mkDeferredModuleOpt "Host-specific NixOS configuration";
        home = mkDeferredModuleOpt "Host-specific home-manager configuration, applied to all users for host.";
        features = mkFeatureListOpt "List of features for the host";
        name = mkOption {
          default = name;
          readOnly = true;
          description = "Hostname";
        };
        users = mkOption {
          type = types.lazyAttrsOf (
            types.submodule {
              options = {
                features = mkFeatureListOpt ''
                  List of features specific to the user and host.

                  While a feature may specify NixOS modules in addition to home
                  modules, only home modules will affect configuration.  For this
                  reason, users should be encouraged to avoid pointlessly specifying
                  their own NixOS modules.
                '';
                home = mkDeferredModuleOpt "User-and-host-specific home configuration";
              };
            }
          );
          default = { };
        };
      };
    }
  );

  makeHost =
    hostName: hostConfig:
    let
      moduleArgs = {
        _module.args = {
          inherit lib';
        };
      };

      nixosModules =
        (collectNixosModules hostConfig.features)
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

      users = lib.mapAttrs (username: userConfig: {
        imports =
          homeModules
          ++ userConfig.home.imports

          # Home modules for this user-and-host-specific intersection.
          ++ (collectHomeModules userConfig.features)

          # Baseline home modules for this user.
          ++ (collectHomeModules config.dotfield.users.${username}.features)

          # User-defined extensions to Dotfield features.
          ++ (collectUserFeatures username)

          # Baseline home configuration for this user.
          ++ [ (config.dotfield.users.${username}.home) ];
      }) hostConfig.users;

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
    };

in
{
  options.dotfield.hosts.nixos = mkOption {
    type = types.attrsOf (types.submodule hostSubmodule);
  };

  config = {
    flake.nixosConfigurations = lib.mapAttrs makeHost config.dotfield.hosts.nixos;
  };
}
