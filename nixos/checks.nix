# SPDX-FileCopyrightText: Copyright (C) 2022-2023 Chris Montgomery
# SPDX-FileCopyrightText: Copyright (C) 2019-2022 Divnix and the Digga contributors
# SPDX-License-Identifier: GPL-3.0-or-later AND MIT
#
##: Sources
# - https://github.com/divnix/digga/blob/main/src/mkFlake/outputs-builder.nix
{
  self,
  lib,
  ...
}: let
  inherit (self.inputs.digga.lib) mkTest;
  inherit
    (builtins)
    listToAttrs
    ;
  inherit
    (lib)
    filterAttrs
    attrValues
    mapAttrs
    foldl
    warnIf
    ;
in {
  perSystem = {system, ...}: {
    checks = let
      customTests = [];
      hostConfigsOnSystem =
        filterAttrs
        (_: host: (host.config.nixpkgs.system == system))
        self.nixosConfigurations;
      createTest = hostName: host: test:
        warnIf (!(test ? name)) ''
          '${hostName}' has a test without a name. To distinguish tests in the flake output
          all tests must have names.
        '' {
          name = "customTestFor-${hostName}-${test.name}";
          value = mkTest host test;
        };
      createTests = hostName: host: (listToAttrs (map
        (createTest hostName host)
        customTests));
    in
      if (hostConfigsOnSystem != [])
      then
        (foldl (a: b: a // b) {}
          (attrValues
            (mapAttrs createTests hostConfigsOnSystem)))
      else {};
  };
}
