# SPDX-FileCopyrightText: 2023 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
# Source: <https://github.com/divnix/std/blob/3e623646d47ec277947c9626d595f43042b94ab9/src/lib/cfg/lefthook.nix>
{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  l = nixpkgs.lib // builtins;
in {
  data = {};
  format = "yaml";
  output = "lefthook.yml";
  packages = [nixpkgs.lefthook];
  hook.extra = d: let
    # Add an extra hook for adding required stages whenever the file changes
    skip_attrs = [
      "colors"
      "extends"
      "skip_output"
      "source_dir"
      "source_dir_local"
    ];
    stages = l.attrNames (l.removeAttrs d skip_attrs);
    stagesStr = l.concatStringsSep " " stages;
  in ''
    # Install configured hooks
    for stage in ${stagesStr}; do
      ${l.getExe nixpkgs.lefthook} add -f "$stage"
    done
  '';
}
