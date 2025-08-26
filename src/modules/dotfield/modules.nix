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
  inherit (lib'.modules) mkAspectNameOpt mkDeferredModuleOpt;
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
      type = types.lazyAttrsOf (
        types.submodule (
          { name, ... }:
          {
            options = {
              _name = mkAspectNameOpt name;
              nixos = mkDeferredModuleOpt "A NixOS module for the \"${name}\" aspect";
              home = mkDeferredModuleOpt "A Home-Manager module for the \"${name}\" aspect";
            };
          }
        )
      );
      description = ''
        Abstract logical groupings for NixOS and home modules defining
        bulk configuration for features with related functionality.
        The relation itself is the aspect.

        Think of "aspect" as a plane of feature-intensity of such
        importance that it is named non-specifically to refer to some
        general characteristic or collection of functions of a system,
        while a "feature" is some specific component that combines to
        assemble an aspect or multiple aspects.

        For example, a hypothetical "multimedia" aspect could be
        comprised of the hypothetical features "pulseaudio", "mpv", and
        "plex-client".

        As another distinction: the actual "graphical" aspect is
        comprised of many features common to any graphical environment,
        including both virtual machines or desktop personal computers.
        The "workstation" aspect is comprised of many features common to
        the latter, and is *always* also "graphical", but does not
        include virtual machines.
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
