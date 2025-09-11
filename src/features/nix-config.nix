# Copyright (C) 2022-2025 Chris Montgomery
# SPDX-License-Identifier: GPL-2.0-or-later

flake@{ lib, inputs, ... }:

{
  aspects.core.nixos =
    {
      config,
      pkgs,
      ...
    }:
    let
      cfg = config.nix;
    in

    {
      environment.systemPackages = [ cfg.package ];

      nix = {
        nixPath = [
          "nixpkgs=${pkgs.path}"
          "home-manager=${inputs.home-manager}"
        ];
        distributedBuilds = true;
        settings = {
          allowed-users = [ "*" ];
          trusted-users = [
            "root"
            "@wheel"
          ];

          ## === Features ===

          experimental-features = [
            "flakes"
            "nix-command"
            "pipe-operators"
          ];

          system-features = flake.config.meta.hosts.${config.networking.hostName}.supportedFeatures;

          ## === Substituters ===

          builders-use-substitutes = true;
          substituters = [
            "https://dotfield.cachix.org"
            "https://nix-community.cachix.org"
          ];
          trusted-substituters = [
            "ssh://eu.nixbuild.net"
            "https://nixpkgs-update.cachix.org/"
          ];
          trusted-public-keys = [
            "dotfield.cachix.org-1:b5H/ucY/9PDARWG9uWA87ZKWUBU+hnfF30amwiXiaNk="
            "nixbuild.net/cdom-1:DU7hcG2k5kj9nC6NUvsOYQNiaI5UXYjjY5gBOccaND4="
            "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
            "nixpkgs-update.cachix.org-1:6y6Z2JdoL3APdu6/+Iy8eZX2ajf09e4EE9SnxSML1W8="
          ];
        };

        ## `nix store optimise`
        settings.auto-optimise-store = false;
        optimise.automatic = true;

        ## `nix store gc`
        gc.dates = lib.mkDefault "weekly";
        gc.automatic = lib.mkDefault true;

      };
    };
}
