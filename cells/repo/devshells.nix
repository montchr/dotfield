# SPDX-FileCopyrightText: 2022-2023 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs std;
  inherit (inputs.cells) presets secrets;
  l = inputs.nixpkgs.lib // builtins;
  pkgs = inputs.nixpkgs;
  name = "dotfield";
in
  l.mapAttrs (_: std.lib.dev.mkShell) {
    default = {...}: {
      inherit name;
      imports = [
        cell.devshellProfiles.default

        # TODO: unusable in its current state
        # cell.devshellProfiles.pre-commit-hooks

        ##: external
        std.std.devshellProfiles.default
      ];
    };
    ci = _: {
      name = "dotfield-ci";
      packages = with pkgs; [
        cachix
        deadnix
        just
        nvd
        shellcheck
        statix
      ];
    };
  }
