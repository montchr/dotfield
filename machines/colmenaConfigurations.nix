# SPDX-FileCopyrightText: 2023 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
# Ref: <https://github.com/zhaofengli/colmena/issues/60#issuecomment-1510496861>
{
  self,
  inputs,
  withSystem,
  ops,
  ...
}: let
  inherit (inputs) colmena;
  l = inputs.nixpkgs.lib // builtins;
  nodes = l.removeAttrs self.nixosConfigurations ["bootstrap-graphical" "freundix"];
in {
  flake.colmena =
    {
      meta = {
        description = "my personal machines";
        # While it's not great to hard-code the system, for now, this option is
        # required, so we use the sane default nixpkgs for `x86_64-linux`.
        # In the end, this setting gets overridden on a per-host basis.
        nixpkgs = withSystem "x86_64-linux" (ctx: ctx.pkgs);
        nodeNixpkgs = l.mapAttrs (_: v: v.pkgs) nodes;
        nodeSpecialArgs = l.mapAttrs (_: v: v._module.specialArgs) nodes;
      };
    }
    // {
      moraine =
        (nodes.moraine.extendModules {
          modules = [
            colmena.nixosModules.deploymentOptions
            {
              deployment = {
                buildOnTarget = true;
                tags = ["@tso"];
                targetHost = ops.metadata.hosts.moraine.ipv6.address;
                targetUser = "anomich";
              };
            }
          ];
        })
        ._module
        .args
        .modules;
    };
}
