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

  aspectSubmodule =
    { name, ... }:
    {
      options = {
        _name = mkOption {
          default = name;
          readOnly = true;
          internal = true;
          description = "Name of the aspect";
        };
        nixos = mkDeferredModuleOpt "A NixOS module for the \"${name}\" aspect";
        home = mkDeferredModuleOpt "A Home-Manager module for the \"${name}\" aspect";
      };
    };

  mkFeatureListOpt =
    description:
    mkOption {
      type = types.listOf (types.submodule aspectSubmodule);
      default = [ ];
    };
in
{
  flake.lib.modules = {
    inherit
      aspectSubmodule
      mkDeferredModuleOpt
      mkFeatureListOpt
      ;
  };
}
