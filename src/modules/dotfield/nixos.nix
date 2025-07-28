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
  inherit (lib'.modules) collectHomeModules collectNixosModules moduleType;
  lib' = self.lib;
in
{
  options.dotfield.hosts.nixos = mkOption {
    type = types.attrsOf (
      types.submodule (
        { name, ... }:
        let
          modules = mkOption {
            type = types.listOf (
              types.submodule {
                options = {
                  nixos = moduleType "A NixOS module";
                  home = moduleType "A Home-Manager module";
                };
              }
            );
            default = [ ];
          };
        in
        {
          options = config.dotfield.options // {
            inherit modules;
            name = mkOption {
              default = name;
              readOnly = true;
            };
            users = mkOption {
              type = types.lazyAttrsOf (
                types.submodule {
                  options = {
                    inherit modules;
                    home = moduleType "User-specific home-manager configuration";
                  };
                }
              );
              default = { };
            };
            nixos = moduleType "Host-specific NixOS configuration";
            home = moduleType "Host-specific home-manager configuration, applied to all users for host.";
          };
        }
      )
    );
  };

  config = {
    flake.nixosConfigurations = lib.mapAttrs (
      hostname: hostConfig:
      let
        moduleArgs = {
          inherit lib';
        };

        nixosModules =
          (collectNixosModules hostConfig.modules)
          ++ hostConfig.nixos.imports
          ++ [
            config.dotfield.nixos
            moduleArgs
          ];

        # TODO: why is this list so different from `nixosModules`?  i
        # can see that the "collection" happens while mapping `users`
        # below, as that is per-user configuration.  but why is there no
        # equivalent for the defaults defined under........ okay, i have
        # more to refactor before i can answer this...
        homeModules = [
          config.dotfield.home
          hostConfig.home
          moduleArgs
        ];

        users = lib.mapAttrs (_: v: {
          imports = (collectHomeModules v.modules) ++ v.home.imports ++ homeModules;
        }) hostConfig.users;

      in
      inputs.nixpkgs.lib.nixosSystem {
        modules = nixosModules ++ [
          inputs.home-manager.nixosModules.default
          {
            home-manager.users = users;
          }
        ];
      }
    ) config.dotfield.hosts.nixos;
  };
}
