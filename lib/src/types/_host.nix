{
  l,
  lib,
  ...
}: let
  inherit
    (lib.types)
    binary-cache-pubkey
    label
    net
    user
    ssh-pubkey
    ssh-privkey
    ;
  t = l.types;
in
  t.submodule ({config, ...}: {
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
  })
