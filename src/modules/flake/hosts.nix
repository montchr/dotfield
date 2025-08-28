# Copyright (C) 2025 Chris Montgomery
# Copyright (C) 2025 Michael Belsanti
# SPDX-License-Identifier: GPL-2.0-or-later OR MIT

{
  lib,
  self,
  ...
}:
let
  inherit (lib) mkOption types;
  inherit (self.lib.modules) mkDeferredModuleOpt mkAspectListOpt;

  hostSubmodule = (
    { name, ... }:
    {
      options = {
        configuration = mkDeferredModuleOpt "NixOS configuration for the host";
        aspects = mkAspectListOpt "List of aspects for the host";
        channel = mkOption {
          type = types.enum [
            "nixos-stable"
            "nixos-unstable"
            "nixpkgs-apple-silicon"
            "nixpkgs-trunk"
            "nixpkgs"
          ];
          default = "nixpkgs";
          description = "Name of the Nixpkgs input the host will be built upon";
        };
        name = mkOption {
          default = name;
          readOnly = true;
          description = "Hostname";
        };
        system = mkOption {
          type = types.enum [
            "aarch64-linux"
            "x86_64-linux"
          ];
          description = "System string for the host";
        };
        baseline = mkOption {
          type = types.submodule {
            options = {
              home = mkDeferredModuleOpt "Host-specific home-manager configuration, applied to all users for host.";
            };
          };
          description = "Baseline configurations for repeatable configuration types on this host";
          default = { };
        };
        users = mkOption {
          type = types.lazyAttrsOf (
            types.submodule {
              options = {
                aspects = mkAspectListOpt ''
                  List of aspects specific to the user and host.

                  While a feature may specify NixOS modules in addition to home
                  modules, only home modules will affect configuration.  For this
                  reason, users should be encouraged to avoid pointlessly specifying
                  their own NixOS modules.
                '';
                configuration = mkDeferredModuleOpt "User-specific home configuration on this host";
              };
            }
          );
          default = { };
          description = "Users on this host";
        };
      };
    }
  );

in
{
  options.hosts = {
    nixos = mkOption {
      type = types.attrsOf (types.submodule hostSubmodule);
      description = "Attributes of Dotfield NixOS host specifications.";
    };
  };
}
