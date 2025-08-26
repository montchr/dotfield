# Copyright (C) 2024-2025 Chris Montgomery
# Copyright (C) 2025 Michael Belsanti
# SPDX-License-Identifier: GPL-2.0-or-later OR MIT

{
  lib,
  config,
  ...
}:
let
  inherit (lib) mkOption types;
  inherit (lib'.modules) aspectSubmodule mkDeferredModuleOpt;
  lib' = config.flake.lib;
in
{
  options.dotfield = {
    baseline = mkOption {
      type = types.submodule {
        options = {
          nixos = mkDeferredModuleOpt "Global NixOS configuration";
          home = mkDeferredModuleOpt "Global Home-Manager configuration";
        };
      };
      description = "Baseline NixOS and home configurations";
    };
    aspects = mkOption {
      type = types.lazyAttrsOf (types.submodule aspectSubmodule);
      description = ''
        Logical groupings for NixOS and home modules definining
        configuration with related functionality
      '';
    };
  };

  config.flake.modules = {
    nixos = (config.dotfield.aspects |> lib.mapAttrs (_: module: module.nixos)) // {
      default.imports = config.dotfield.baseline.nixos.imports;
    };
    home = (config.dotfield.aspects |> lib.mapAttrs (_: module: module.home)) // {
      default.imports = config.dotfield.baseline.home.imports;
    };
  };
}
