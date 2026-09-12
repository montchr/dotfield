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

      # Disable nix channels. Use flakes instead.
      nix.channel.enable = lib.mkDefault false;
      nix.nixPath = [
        "nixpkgs=${pkgs.path}"
        "home-manager=${inputs.home-manager}"
      ];
      nix.distributedBuilds = true;

      nix.settings.allowed-users = [ "*" ];
      nix.settings.trusted-users = [
        "root"
        "@wheel"
      ];

      nix.settings.experimental-features = [
        "flakes"
        "nix-command"
        (if cfg.package.pname == "lix" then "pipe-operator" else "pipe-operators")
      ]
      ++ lib.optional (lib.versionOlder (lib.versions.majorMinor config.nix.package.version) "2.22") "repl-flake";

      nix.settings.system-features =
        flake.config.meta.hosts.${config.networking.hostName}.supportedFeatures;

      # The default at 10 is rarely enough.
      nix.settings.log-lines = lib.mkDefault 25;

      # Avoid disk full issues.
      nix.settings.max-free = lib.mkDefault (20 * 1024 * 1024 * 1024);
      nix.settings.min-free = lib.mkDefault (5 * 1024 * 1024 * 1024);
    };
}
