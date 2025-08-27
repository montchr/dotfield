# Copyright (C) 2025 Chris Montgomery
# Copyright (C) 2025 Michael Belsanti
# SPDX-License-Identifier: GPL-2.0-or-later OR MIT

{
  lib,
  self,
  ...
}:
let
  inherit (lib) mkOption types;

  hostSubmodule = (
    { name, ... }:
    {
      options = {
        configuration = mkOption {
          type = types.deferredModule;
          description = "Host-specific NixOS configuration";
          default = { };
        };
        channel = mkOption {
          type = types.enum [
            "nixos-stable"
            "nixos-unstable"
            "nixpkgs-apple-silicon"
            "nixpkgs-trunk"
          ];
          default = "nixos-unstable";
          description = "Name of the Nixpkgs input the host will be built upon";
        };
        name = mkOption {
          default = name;
          readOnly = true;
          description = "Hostname";
        };
        system = mkOption {
          type = types.enum [
            "aarch64-linux"
            "x86_64-linux"
          ];
          description = "System string for the host";
        };
      };
    }
  );

in
{
  options.hosts.nixos = mkOption {
    type = types.attrsOf (types.submodule hostSubmodule);
    description = ''
      Attributes of NixOS host specifications.
    '';
  };
}
