# SPDX-FileCopyrightText: 2023 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
# Ref: <https://github.com/zhaofengli/colmena/issues/60#issuecomment-1510496861>
#
# TODO: make sure this does not confuse the `colmena` CLI, which might give
# //hive.nix special treatment.
{ self, inputs, ... }:
let
  inherit (self.lib.colmena) mkNode metaFor;
  l = inputs.nixpkgs.lib // builtins;

  configurations = l.removeAttrs self.nixosConfigurations [
    "bootstrap-graphical"
    "freundix"
  ];

  mkNode' = n: mkNode configurations.${n} n;

  mkHive = nodes: (l.mapAttrs mkNode' nodes) // (metaFor (l.intersectAttrs nodes configurations));
in
{
  flake.colmena = mkHive {
    gabbro = {
      tags = [
        "@seadome"
        "@loopgarden"
        "@hetznerCloud"
        "@fsn-1"
      ];
    };
    hierophant = {
      tags = [
        "@seadome"
        "@loopgarden"
        "@hetznerCloud"
        "@us-east"
      ];
    };
    moraine = {
      tags = [
        "@seadome"
        "@tso"
      ];
      targetUser = "anomich";
    };
  };
}
