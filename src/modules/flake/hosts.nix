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
  inherit (self.lib.modules) mkDeferredModuleOpt;

  hostSubmodule = (
    { name, ... }:
    {
      options = {
        configuration = mkDeferredModuleOpt "NixOS configuration for the host";
        channel = mkOption {
          type = types.enum [
            "nixos-stable"
            "nixos-unstable"
            "nixpkgs-apple-silicon"
            "nixpkgs-trunk"
          ];
          default = "nixos-unstable";
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
              home = mkDeferredModuleOpt "Host-specific baseline home-manager configuration";
            };
          };
          description = ''
            Baseline configurations for repeatable configuration scopes on this
            host.

            A baseline configuration is a minimal configuration to be
            applied to all instances of a repeatable configuration scope
            on the host.
          '';
          default = { };
        };
        users = mkOption {
          type = types.lazyAttrsOf (
            types.submodule {
              options = {
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
