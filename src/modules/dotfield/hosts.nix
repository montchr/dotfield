# Copyright (C) 2024-2025 Chris Montgomery
# Copyright (C) 2025 Michael Belsanti
# SPDX-License-Identifier: GPL-2.0-or-later OR MIT

{
  config,
  lib,
  self,
  ...
}:
let
  inherit (lib) mkOption types;
  inherit (lib'.modules) mkDeferredModuleOpt mkFeatureListOpt;
  lib' = self.lib;

  hostSubmodule = (
    { name, ... }:
    {
      options = {
        nixos = mkDeferredModuleOpt "Host-specific NixOS configuration";
        aspects = mkFeatureListOpt "List of aspects for the host";
        baseline = mkOption {
          type = types.submodule {
            options = {
              home = mkDeferredModuleOpt "Host-specific home-manager configuration, applied to all users for host.";
            };
          };
          description = "Baseline configurations for repeatable configuration types on this host";
          default = { };
        };
        name = mkOption {
          default = name;
          readOnly = true;
          description = "Hostname";
        };
        users = mkOption {
          type = types.lazyAttrsOf (
            types.submodule {
              options = {
                aspects = mkFeatureListOpt ''
                  List of aspects specific to the user and host.

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

in
{
  options.dotfield.hosts.nixos = mkOption {
    type = types.attrsOf (types.submodule hostSubmodule);
  };
}
