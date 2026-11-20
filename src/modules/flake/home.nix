# SPDX-FileCopyrightText: (C) 2026 Chris Montgomery <chmont@protonmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# Standalone Home Manager configurations for faster user-level switching.
#
# This module creates `homeConfigurations` outputs that can be switched
# independently of NixOS using `nh home switch -s <specialisation>`.
{
  config,
  lib,
  inputs,
  self,
  ...
}:
let
  inherit (self.lib.modules)
    collectRequires
    resolveUserHomeModules
    resolveUserOverlays
    ;

  # Build standalone homeConfigurations for each user on each host.
  makeHomeConfigurations =
    hostName: hostSpec:
    let
      inherit (hostSpec) system;

      hostAspectDeps = collectRequires config.aspects hostSpec.aspects;
      hostAspects = hostSpec.aspects ++ hostAspectDeps;

      homeModules = [
        inputs.sops-nix.homeManagerModules.sops
        inputs.stylix.homeModules.stylix
        config.aspects.core.home
        hostSpec.baseline.home
      ];

      makeHome = username: hostedUserSpec: {
        name = "${username}@${hostName}";
        value = inputs.home-manager.lib.homeManagerConfiguration {
          pkgs = import inputs.${hostSpec.channel} {
            inherit system;
            config = self.lib.nixpkgsConfig;
            overlays = resolveUserOverlays {
              inherit username hostAspects hostedUserSpec;
            };
          };
          extraSpecialArgs = {
            inherit inputs self;
            flake = config;
          };
          modules = [
            {
              home.username = lib.mkDefault username;
              home.homeDirectory = lib.mkDefault "/home/${username}";
            }
          ]
          ++ resolveUserHomeModules {
            inherit username hostAspects hostedUserSpec;
            baseHomeModules = homeModules;
          };
        };
      };
    in
    lib.mapAttrsToList makeHome hostSpec.users;
in
{
  flake.homeConfigurations = builtins.listToAttrs (
    lib.flatten (lib.mapAttrsToList makeHomeConfigurations config.hosts.nixos)
  );
}
