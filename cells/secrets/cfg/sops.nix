# SPDX-FileCopyrightText: 2022-2023 Chris Montgomery <chris@cdom.io>
# SPDX-License-Identifier: GPL-3.0-or-later
{
  inputs,
  cell,
}: let
  inherit (inputs.cells.ops.data) keys;

  l = inputs.nixpkgs.lib // builtins;

  adminKeys = {
    # NOTE: age-plugin-yubikey is not supported by sops.
    #       including it in a list of keys here will result in an error.
    age = [keys.age.cdom-age];
    pgp = [keys.pgp.ids."0x135EEDD0F71934F3"];
  };

  defaultPathPattern = "secrets/[^/]+\.yaml$";
  prefixedPathPattern = s: "machines/${s}/${defaultPathPattern}";

  mkHostRule = hostName: {
    path_regex = prefixedPathPattern hostName;
    # NOTE: Each rule should have only *one* key group!
    #       Unless you're absolutely sure you mean to use multiple key groups...
    #       Key groups, which use the Shamir's Secret Sharing algorithm,
    #                   are a feature used to increase the security of a secret
    #                   by requiring *multiple* keys in order to decrypt a secret.
    #       That is, the data will be _split_ across each group,
    #       requiring consensus to decrypt.
    #       Reference: <https://github.com/getsops/sops#214key-groups>
    key_groups = l.singleton {
      age =
        [keys.age.${hostName}]
        ++ adminKeys.age;
      pgp = [] ++ adminKeys.pgp;
    };
  };

  # TODO: should automate this somehow
  hosts = [
    "boschic"
    "hierophant"
    "hodgepodge"
    "moraine"
    "ryosuke"
  ];
in {
  # NOTE: The top-level `keys` map referenced in sops-nix documentation is not important here.
  #       It is not actually used in sops configuration itself.
  #       It is used in sops-nix examples as an arbitrary holding area for YAML anchors,
  #       which we have no need for in Nix.
  #       <https://github.com/getsops/sops/blob/master/config/config.go>
  # NOTE: Creation rules are evaluated sequentially. The first match wins.
  #       For this reason, it is important that creation rules are listed
  #       from *most* to *least* specific.
  #       <https://github.com/getsops/sops#using-sops-yaml-conf-to-select-kms-pgp-for-new-files>
  creation_rules =
    (l.map mkHostRule hosts)
    ++ [
      # TODO: accomodate structure variations like this with dmerge
      {
        path_regex = prefixedPathPattern "tuvix";
        key_groups = l.singleton {
          age =
            [
              keys.age.tuvix
              keys.age.cdom-at-tuvix
            ]
            ++ adminKeys.age;
          pgp = [] ++ adminKeys.pgp;
        };
      }

      ##: common secrets
      {
        path_regex = defaultPathPattern;
        key_groups = l.singleton {
          age = [] ++ (l.map (v: keys.age.${v}) hosts) ++ adminKeys.age;
          pgp = [] ++ adminKeys.pgp;
        };
      }
    ];
}
