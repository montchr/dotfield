{ config, osConfig, options, lib, pkgs, ... }:

with lib;

let
  cfg = config.age;

  # we need at least rage 0.5.0 to support ssh keys
  rage =
    if lib.versionOlder pkgs.rage.version "0.5.0"
    then pkgs.callPackage ../pkgs/rage.nix { }
    else pkgs.rage;
  ageBin = "${rage}/bin/rage";

  identities = builtins.concatStringsSep " " (map (path: "-i ${path}") cfg.identityPaths);

  installSecret = secretType: ''
    echo "decrypting ${secretType.file} to ${secretType.path}..."
    TMP_FILE="${secretType.path}.tmp"
    $DRY_RUN_CMD mkdir $VERBOSE_ARG -p $(dirname ${secretType.path})
    (
      $DRY_RUN_CMD umask u=r,g=,o=
      $DRY_RUN_CMD ${ageBin} --decrypt ${identities} -o "$TMP_FILE" "${secretType.file}"
    )
    $DRY_RUN_CMD chmod $VERBOSE_ARG ${secretType.mode} "$TMP_FILE"
    $DRY_RUN_CMD chown $VERBOSE_ARG ${secretType.owner}:${secretType.group} "$TMP_FILE"
    $DRY_RUN_CMD mv $VERBOSE_ARG -f "$TMP_FILE" "${secretType.path}"
  '';

  installSecrets = builtins.concatStringsSep "\n" ([ "echo '[agenix] decrypting user secrets...'" ] ++ (map installSecret (attrValues cfg.secrets)));

  secretType = types.submodule ({ config, ... }: {
    options = {
      name = mkOption {
        type = types.str;
        default = config._module.args.name;
        description = ''
          Name of the file used in /run/user/<uid>/secrets
        '';
      };
      file = mkOption {
        type = types.path;
        description = ''
          Age file the secret is loaded from.
        '';
      };
      path = mkOption {
        type = types.str;
        default = "/run/user/$UID/secrets/${config.name}";
        description = ''
          Path where the decrypted secret is installed.
        '';
      };
      mode = mkOption {
        type = types.str;
        default = "0400";
        description = ''
          Permissions mode of the in octal.
        '';
      };
      owner = mkOption {
        type = types.str;
        default = "$UID";
        description = ''
          User of the file.
        '';
      };
      group = mkOption {
        type = types.str;
        default = "$(id -g)";
        description = ''
          Group of the file.
        '';
      };
    };
  });
in
{
  options.age = {
    secrets = mkOption {
      type = types.attrsOf secretType;
      default = { };
      description = ''
        Attrset of secrets.
      '';
    };
    identityPaths = mkOption {
      type = types.listOf types.path;
      default = osConfig.age.identityPaths or [ ];
      description = ''
        Path to SSH keys to be used as identities in age decryption.
      '';
    };
  };
  config = mkIf (cfg.secrets != { }) (mkMerge [

    {
      assertions = [{
        assertion = cfg.identityPaths != [ ];
        message = "age.identityPaths must be set.";
      }];

      home.activation.agenix = hm.dag.entryAfter [ "writeBoundary" ] installSecrets;
    }
  ]);
}

##: source:
# https://github.com/ryantm/agenix/blob/1c11ad7b11a5b09c28b7892fbbe984274e7031ed/modules/hm-age.nix
