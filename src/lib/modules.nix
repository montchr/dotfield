# Copyright (C) 2024-2025 Chris Montgomery
# Copyright (C) 2025 Michael Belsanti
# SPDX-License-Identifier: GPL-2.0-or-later OR MIT

{ lib, ... }:
let
  inherit (lib) mkOption types;

  mkDeferredModuleOpt =
    description:
    mkOption {
      inherit description;
      type = types.deferredModule;
      default = { };
    };

  aspectsubmodule = {
    options = {
      nixos = mkDeferredModuleOpt "A NixOS module";
      home = mkDeferredModuleOpt "A Home-Manager module";
    };
  };

  mkFeatureListOpt =
    description:
    mkOption {
      type = types.listOf (types.submodule aspectsubmodule);
      default = [ ];
    };
in
{
  flake.lib.modules = {
    inherit
      aspectsubmodule
      mkDeferredModuleOpt
      mkFeatureListOpt
      ;
  };
}
