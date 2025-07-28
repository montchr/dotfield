# Copyright (C) 2024-2025 Chris Montgomery
# Copyright (C) 2025 Michael Belsanti
# SPDX-License-Identifier: GPL-2.0-or-later OR MIT

{
  self,
  lib,
  config,
  ...
}:
let
  inherit (lib) mkOption types;
  inherit (lib'.modules) moduleType;
  lib' = self.lib;
in
{
  options.dotfield = {
    nixos = moduleType "Global NixOS configuration";
    home = moduleType "Global Home-Manager configuration";
    modules = mkOption {
      type = types.lazyAttrsOf (
        types.submodule {
          options = {
            nixos = moduleType "A NixOS module";
            home = moduleType "A Home-Manager module";
          };
        }
      );
    };
  };

  config.flake.modules = {
    nixos = lib.mapAttrs (moduleName: moduleConfig: moduleConfig.nixos) config.dotfield.modules // {
      default.imports = config.dotfield.nixos.imports;
    };
    home = lib.mapAttrs (moduleName: moduleConfig: moduleConfig.home) config.dotfield.modules // {
      default.imports = config.dotfield.home.imports;
    };
  };
}
