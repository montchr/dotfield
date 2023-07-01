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
    # FIXME: git commits in magit to hang for a while during hooks
    #        affects both dotfield and apparat
    #        it might be my emacs config, but i haven't paid much attention to this devshell for a while
    default = {...}: {
      inherit name;
      imports = [
        cell.devshellProfiles.default
        secrets.devshellProfiles.default

        ##: external
        std.std.devshellProfiles.default
      ];
      nixago = [
        (presets.cfg.commitlint {})
        (presets.cfg.editorconfig {})
        (presets.cfg.lefthook {})
        (presets.cfg.prettier {})
        (presets.cfg.treefmt {})
        (presets.cfg.statix {
          data = {
            disabled = ["useless_parens"];
          };
        })
        (presets.cfg.stylua {})
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
