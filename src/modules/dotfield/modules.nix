# Copyright (C) 2024-2025 Chris Montgomery
# Copyright (C) 2025 Michael Belsanti
# SPDX-License-Identifier: GPL-2.0-or-later OR MIT

{
  lib,
  config,
  ...
}:
let
  inherit (lib'.modules) scopedModulesOptions;
  lib' = config.flake.lib;
in
{
  options.dotfield = scopedModulesOptions;

  config.flake.modules = {
    nixos = (config.dotfield.modules |> lib.mapAttrs (_: module: module.nixos)) // {
      default.imports = config.dotfield.nixos.imports;
    };
    home = (config.dotfield.modules |> lib.mapAttrs (_: module: module.home)) // {
      default.imports = config.dotfield.home.imports;
    };
  };
}
