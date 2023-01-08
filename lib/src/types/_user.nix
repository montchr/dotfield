{
  l,
  lib,
  ...
}: let
  inherit
    (lib)
    absolute-pathname
    genid_uint31
    pgp-pubkey
    ssh-pubkey
    username
    ;
  t = l.types;
in
  t.submodule ({config, ...}: {
    options = {
      home = l.mkOption {
        type = absolute-pathname;
        # homePrefix = ifThenElse isDarwin "/Users" "/home";
        default = "/home/${config.name}";
        defaultText = "/home/‹name›";
      };
      mail = l.mkOption {
        type = t.nullOr t.str;
        default = null;
      };
      name = l.mkOption {
        type = username;
        default = config._module.args.name;
      };
      pgp.pubkeys = l.mkOption {
        type = t.attrsOf pgp-pubkey;
        default = {};
        description = ''
          Set of user's PGP public keys.

          Modules supporting PGP may use well-known key names to define
          default values for options, in which case the well-known name
          should be documented in the respective option's description.
        '';
      };
      pubkey = l.mkOption {
        type = t.nullOr ssh-pubkey;
        default = null;
      };
      uid = l.mkOption {
        type = t.int;
        default = genid_uint31 config.name;
        defaultText = "genid_uint31 ‹name›";
      };
    };
  })
