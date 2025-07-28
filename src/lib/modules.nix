# Copyright (C) 2024-2025 Chris Montgomery
# Copyright (C) 2025 Michael Belsanti
# SPDX-License-Identifier: GPL-2.0-or-later OR MIT

{ lib, ... }:
let
  inherit (lib) mkOption types;

  mkDeferredModuleOption =
    description:
    mkOption {
      inherit description;
      type = types.deferredModule;
      default = { };
    };

  scopedSubmoduleType = types.submodule {
    options = {
      nixos = mkDeferredModuleOption "A NixOS module";
      home = mkDeferredModuleOption "A Home-Manager module";
    };
  };

  scopedModulesOptions = {
    nixos = mkDeferredModuleOption "Global NixOS configuration";
    home = mkDeferredModuleOption "Global Home-Manager configuration";
    modules = mkOption {
      type = types.lazyAttrsOf scopedSubmoduleType;
    };
  };
in
{
  flake.lib.modules = {
    inherit
      mkDeferredModuleOption
      scopedModulesOptions
      scopedSubmoduleType
      ;
  };
}
