###: Sources:
#
# - <https://cgit.krebsco.de/stockholm/tree/lib/types.nix?id=4adca5a98417c089e36d27f5a33c454d3d4ff3d8>
# - <https://git.thalheim.io/Mic92/stockholm/src/commit/0b2952f4ed9572521f7c4a21904943ac33c602b0/lib/types.nix>
#
###: Notes:
#
# - References to `retiolum` and `wiregrill` should be replaced accordingly
# - Intentionally removed `net.tinc` option -- I don't expect to use this.
#
{
  l,
  lself,
  ...
}: let
  inherit (lself.ids) genid_uint31;
  inherit (lself.net) normalize-ip6-addr;
  inherit (lself.strings) test;
  t = l.types;
in rec {
  uint = l.mkOptionType {
    name = "unsigned integer";
    check = x: l.isInt x && x >= 0;
    merge = l.mergeOneOption;
  };

  binary-cache-pubkey = t.str;
  pgp-pubkey = t.str;
  ssh-pubkey = t.str;
  ssh-privkey = t.submodule {
    options = {
      bits = l.mkOption {
        type = t.nullOr (t.enum ["4096"]);
        default = null;
      };
      path = l.mkOption {
        type = t.either t.path t.str;
        apply = x:
          {
            path = l.toString x;
            string = x;
          }
          .${l.typeOf x};
      };
      type = l.mkOption {
        type = t.enum ["rsa" "ed25519"];
        default = "ed25519";
      };
    };
  };
  wireguard-pubkey = t.str;

  # RFC952, B. Lexical grammar, <hname>
  hostname = l.mkOptionType {
    name = "hostname";
    check = x: l.isString x && l.all label.check (l.splitString "." x);
    merge = l.mergeOneOption;
  };

  # RFC952, B. Lexical grammar, <name>
  # RFC1123, 2.1  Host Names and Numbers
  label = l.mkOptionType {
    name = "label";
    # TODO(stockholm): case-insensitive labels
    check = test "[0-9A-Za-z]([0-9A-Za-z-]*[0-9A-Za-z])?";
    merge = l.mergeOneOption;
  };

  addr = t.either addr4 addr6;
  addr4 = l.mkOptionType {
    name = "IPv4 address";
    check = let
      IPv4address = let
        d = "([1-9]?[0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])";
      in
        l.concatMapStringsSep "." (l.const d) (l.range 1 4);
    in
      test IPv4address;
    merge = l.mergeOneOption;
  };
  addr6 = l.mkOptionType {
    name = "IPv6 address";
    check = let
      # TODO(stockholm): check IPv6 address harder
      IPv6address = "[0-9a-f.:]+";
    in
      test IPv6address;
    merge = l.mergeOneOption;
  };

  cidr = t.either cidr4 cidr6;
  cidr4 = l.mkOptionType {
    name = "CIDRv4 address";
    check = let
      CIDRv4address = let
        d = "([1-9]?[0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])";
      in
        l.concatMapStringsSep "." (l.const d) (l.range 1 4) + "(/([1-2]?[0-9]|3[0-2]))?";
    in
      test CIDRv4address;
    merge = l.mergeOneOption;
  };
  cidr6 = l.mkOptionType {
    name = "CIDRv6 address";
    check = let
      # TODO(stockholm): check IPv6 address harder
      CIDRv6address = "[0-9a-f.:]+(/([0-9][0-9]?|1[0-2][0-8]))?";
    in
      test CIDRv6address;
    merge = l.mergeOneOption;
  };

  # POSIX.1‐2017, 3.281 Portable Filename
  filename = l.mkOptionType {
    name = "POSIX portable filename";
    check = test "[0-9A-Za-z._][0-9A-Za-z._-]*";
    merge = l.mergeOneOption;
  };

  # POSIX.1‐2017, 3.2 Absolute Pathname
  absolute-pathname = l.mkOptionType {
    name = "POSIX absolute pathname";
    check = x: l.isString x && l.substring 0 1 x == "/" && pathname.check x;
    merge = l.mergeOneOption;
  };

  # POSIX.1-2017, 3.271 Pathname
  pathname = l.mkOptionType {
    name = "POSIX pathname";
    check = x: let
      # The filter is used to normalize paths, i.e. to remove duplicated and
      # trailing slashes.  It also removes leading slashes, thus we have to
      # check for "/" explicitly below.
      xs = l.filter (s: l.stringLength s > 0) (l.splitString "/" x);
    in
      l.isString x && (x == "/" || (l.length xs > 0 && l.all filename.check xs));
    merge = l.mergeOneOption;
  };

  # POSIX.1-2017, 3.216 Login Name
  username = l.mkOptionType {
    inherit (filename) check;
    name = "POSIX login name";
    merge = l.mergeOneOption;
  };

  user = t.submodule ({config, ...}: {
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
  });

  net = t.submodule ({config, ...}: {
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
  });

  host = t.submodule ({config, ...}: {
    options = {
      name = l.mkOption {
        type = label;
        default = config._module.args.name;
      };
      nets = l.mkOption {
        type = t.attrsOf net;
        default = {};
      };

      binary-cache.pubkey = l.mkOption {
        type = t.nullOr binary-cache-pubkey;
        default = null;
      };

      ci = l.mkOption {
        description = ''
          Whether the host wants to be tested by some CI system.
          See ‹stockholm/krebs/2configs/buildbot-all.nix›
        '';
        type = t.bool;
        default = false;
      };

      external = l.mkOption {
        description = ''
          Whether the host is defined externally (in contrast to being defined
          in ‹dotfield›).  This is useful e.g. when legacy and/or adopted
          hosts should be part of <retiolum> or some other component.
        '';
        type = t.bool;
        default = false;
      };

      monitoring = l.mkOption {
        description = ''
          Whether the host should be monitored by monitoring tools like Prometheus.
        '';
        type = t.bool;
        default = false;
      };

      owner = l.mkOption {
        type = user;
      };

      extraZones = l.mkOption {
        default = {};
        type = t.attrsOf (t.enum ["A" "AAAA" "MX" "NS"]);
      };

      secure = l.mkOption {
        type = t.bool;
        default = false;
        description = ''
          Whether the host is capable of keeping secret information.

          TODO(stockholm): define minimum requirements for secure hosts
        '';
      };

      ssh.pubkey = l.mkOption {
        type = t.nullOr ssh-pubkey;
        default = null;
      };

      ssh.privkey = l.mkOption {
        type = t.nullOr ssh-privkey;
        default = null;
      };

      syncthing.id = l.mkOption {
        # TODO(stockholm): syncthing id type
        type = t.nullOr t.str;
        default = null;
      };

      # TODO: remove unless using Hashicorp Consul
      # consul = l.mkOption {
      #   description = ''
      #     Whether the host is a member of the global consul network
      #   '';
      #   type = t.bool;
      #   default = false;
      # };
    };
  });

  # TODO(cdom): consider reworking this if it doesn't fit with our approach
  fileLocation = t.submodule {
    options = {
      # TODO(stockholm): user
      host = l.mkOption {
        type = host;
      };
      # TODO(stockholm): merge with ssl.privkey.path
      path = l.mkOption {
        type = t.either t.path t.str;
        apply = x:
          {
            path = toString x;
            string = x;
          }
          .${l.typeOf x};
      };
    };
  };
}
