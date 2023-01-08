{
  l,
  lib,
  ...
}: let
  inherit (lib) normalize-ip6-addr;
  inherit
    (lib.types)
    addr
    addr4
    addr6
    cidr4
    cidr6
    hostname
    label
    net
    uint
    ;
  t = l.types;
in
  t.submodule ({config, ...}: {
    options = {
      name = l.mkOption {
        type = label;
        default = config._module.args.name;
      };
      via = l.mkOption {
        type =
          # XXX break infinite recursion when generating manuals
          if config._module.args.name == "‹name›"
          then
            l.mkOptionType {
              name = "‹net›";
            }
          else t.nullOr net;
        default = null;
      };
      addrs = l.mkOption {
        type = t.listOf (t.either addr t.str);
        default =
          l.optional (config.ip4 != null) config.ip4.addr
          ++ l.optional (config.ip6 != null) config.ip6.addr;
      };
      aliases = l.mkOption {
        # TODO(stockholm): nonEmptyListOf hostname
        type = t.listOf hostname;
        default = [];
      };
      mac = l.mkOption {
        type = t.nullOr t.str;
        default = null;
      };
      ip4 = l.mkOption {
        type = t.nullOr (t.submodule (ip4: {
          options = {
            addr = l.mkOption {
              type = addr4;
            };
            prefix = l.mkOption ({type = cidr4;}
              // {
                # FIXME(dotfield)
                # retiolum.default = "10.243.0.0/16";
                # wiregrill.default = "10.244.0.0/16";
              }
              .${config._module.args.name}
              or {
                default = "${ip4.config.addr}/32";
              });
            prefixLength = l.mkOption ({
                type = uint;
              }
              // {
                # FIXME(dotfield)
                # retiolum.default = 16;
                # wiregrill.default = 16;
              }
              .${config._module.args.name}
              or {
                default = 32;
              });
          };
        }));
        default = null;
      };
      ip6 = l.mkOption {
        type = t.nullOr (t.submodule (ip6: {
          options = {
            addr = l.mkOption {
              type = addr6;
              apply = normalize-ip6-addr;
            };
            prefix = l.mkOption ({
                type = cidr6;
              }
              // {
                # FIXME(dotfield)
                # retiolum.default = "42:0::/32";
                # wiregrill.default = "42:1::/32";
              }
              .${config._module.args.name}
              or {
                default = "${ip6.config.addr}/128";
              });
            prefixLength = l.mkOption ({
                type = uint;
              }
              // {
                # FIXME(dotfield)
                # retiolum.default = 32;
                # wiregrill.default = 32;
              }
              .${config._module.args.name}
              or {
                default = 128;
              });
          };
        }));
        default = null;
      };
      ssh = l.mkOption {
        type = t.submodule {
          options = {
            port = l.mkOption {
              type = t.int;
              default = 22;
            };
          };
        };
        default = {};
      };
      wireguard = l.mkOption {
        type = t.nullOr (t.submodule ({config, ...}: {
          options = {
            port = l.mkOption {
              type = t.int;
              description = "tinc port to use to connect to host";
              default = 51820;
            };
            pubkey = l.mkOption {
              type = wireguard-pubkey;
            };
            subnets = l.mkOption {
              type = t.listOf cidr;
              description = ''
                wireguard subnets,
                this defines how routing behaves for hosts that can't reach each other.
              '';
              default = [];
            };
          };
        }));
        default = null;
      };
    };
  })
