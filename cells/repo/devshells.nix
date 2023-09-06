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
      nixago = [
        (presets.cfg.commitlint {})
        (presets.cfg.editorconfig {})
        (presets.cfg.prettier {})
        (presets.cfg.treefmt {})
        (presets.cfg.statix {
          data = {
            disabled = ["useless_parens"];
          };
        })
        (presets.cfg.stylua {})

        (secrets.cfg.sops {})

        # FIXME: git commits in magit to hang for a while during hooks
        #        possibly related: <https://github.com/evilmartians/lefthook/issues/510>
        # (presets.cfg.lefthook {})
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
