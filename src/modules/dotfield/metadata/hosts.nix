{
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

  userSubmodule = types.submodule {
    options = {
      age = mkOption {
        type = types.str;
        description = "Age public key for the user";
      };
      keys = mkOption {
        type = types.listOf types.str;
        description = "SSH public keys for the user";
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

  hostMetadataSubmodule = types.submodule (
    { name, config, ... }:
    let
      hostname = name;
      keysDir = ./../../hosts/${hostname}/keys;

      # Function to get SSH public key files
      getSshKeys =
        dir:
        if builtins.pathExists dir then
          builtins.readDir dir
          |> lib.filterAttrs (
            name: type: type == "regular" && lib.hasSuffix ".ssh.pub" name
          )
          |> lib.mapAttrsToList (filename: _: builtins.readFile (dir + "/${filename}"))
        else
          [ ];
    in
    {
      options = {
        age = mkOption {
          type = types.str;
          description = "Age public key for the host";
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
          type = types.listOf types.str;
          default = [ ];
          description = ''
            SSH public keys for the host.

            By default, this will automatically include the contents of all .ssh.pub files
            in src/hosts/<hostname>/keys/.
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

      config = {
        keys = lib.mkDefault (getSshKeys keysDir);
      };
    }
  );
in
{
  options.dotfield.metadata.hosts = mkOption {
    type = types.attrsOf hostMetadataSubmodule;
    default = { };
    description = "Host metadata configuration including hardware specs, networking, and user keys";
  };
}
