# SPDX-FileCopyrightText: 2022-2023 Chris Montgomery <chris@cdom.io>
#
# SPDX-License-Identifier: GPL-3.0-or-later
{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs std;
  inherit (inputs.cells) lib presets;
  l = inputs.nixpkgs.lib // builtins;
  pkgs = inputs.nixpkgs;
  name = "dotfield";
in
  l.mapAttrs (_: std.lib.dev.mkShell) {
    default = {...}: {
      inherit name;
      imports = [
        std.std.devshellProfiles.default
        cell.devshellProfiles.default
      ];
      nixago = [
        (presets.nixago.commitlint {})
        (presets.nixago.editorconfig {})
        (cell.nixago.garnix {
          hook.mode = "copy";
        })
        (presets.nixago.lefthook {})
        (presets.nixago.prettier {})
        (presets.nixago.treefmt {})
        (presets.nixago.statix {
          configData = {
            disabled = ["useless_parens"];
          };
        })
        (presets.nixago.stylua {})
      ];
      packages = [
        nixpkgs.deadnix
        nixpkgs.gh
        nixpkgs.reuse
        nixpkgs.statix
        nixpkgs.treefmt
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
