{
  config,
  lib,
  ...
}:
let
  inherit (lib) mkOption types;

  ipv4Submodule = types.submodule {
    options = {
      address = mkOption {
        type = types.str;
        description = "IPv4 address of the host";
      };
    };
  };

  networkSubmodule = types.submodule {
    options = {
      ipv4 = mkOption {
        type = ipv4Submodule;
        description = "IPv4 network configuration";
      };
    };
  };

  hardwareSubmodule = types.submodule {
    options = {
      mem = mkOption {
        type = types.int;
        description = "Memory in GB";
      };
      vcpus = mkOption {
        type = types.int;
        description = "Number of virtual CPUs";
      };
      system = mkOption {
        type = types.str;
        description = "System architecture (e.g., x86_64-linux, aarch64-linux)";
      };
    };
  };

  keysSubmodule = types.submodule {
    options = {
      age = mkOption {
        type = with types; str;
        description = "Age public key";
        default = [ ];
      };
      ssh = mkOption {
        type = with types; listOf str;
        description = "(Required) SSH public keys";
      };
    };
  };

  userSubmodule = types.submodule {
    options = {
      keys = mkOption {
        type = keysSubmodule;
        description = "Public keys for the user";
        default = { };
      };
    };
  };

  syncthingSubmodule = types.submodule {
    options = {
      id = mkOption {
        type = types.str;
        description = "Syncthing device ID";
      };
    };
  };

  hostMetadataSubmodule = types.submodule {
    options = {
      admins = mkOption {
        type = with types; listOf str;
        description = "Administrative users for the host";
        default = [ "cdom" ];
      };

      ipv4 = mkOption {
        type = ipv4Submodule;
        description = "IPv4 configuration for the host";
      };

      hardware = mkOption {
        type = hardwareSubmodule;
        description = "Hardware specifications";
      };

      keys = mkOption {
        type = keysSubmodule;
        default = { };
        description = ''
          Public keys for the host.
        '';
      };

      network = mkOption {
        type = types.str;
        description = "Network name/identifier";
      };

      networks = mkOption {
        type = types.attrsOf networkSubmodule;
        default = { };
        description = "Network configurations (e.g., tailscale, home, etc.)";
      };

      users = mkOption {
        type = types.attrsOf userSubmodule;
        default = { };
        description = "User configurations for this host";
      };

      syncthing = mkOption {
        type = types.nullOr syncthingSubmodule;
        default = null;
        description = "Syncthing configuration";
      };
    };
  };
in
{
  options.dotfield.meta.hosts = mkOption {
    type = types.lazyAttrsOf hostMetadataSubmodule;
    default = { };
    description = "Host metadata";
  };
}
