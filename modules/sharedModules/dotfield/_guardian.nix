{
  options,
  config,
  lib,
  ...
}: let
  inherit (builtins) hasAttr;
  inherit
    (lib)
    mkAliasDefinitions
    mkEnableOption
    mkIf
    mkOption
    optional
    types
    ;

  cfg = config.dotfield.guardian;
in {
  options.dotfield.guardian = {
    enable = mkOption {
      default = true;
      type = types.bool;
      description = "Whether to designate a guardian user for this system.";
    };

    username = mkOption {
      type = with types; nullOr str;
      default = null;
      description = ''
        Name of the guardian user. Must be an existing non-system user.
      '';
    };
    user = mkOption {
      readOnly = true;
    };
    keys = {
      all = mkOption {
        type = with types; listOf str;
        default = [];
      };
    };
    autoLogin = mkEnableOption "Whether to log the guardian user in automatically.";
  };
  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.username != null;
        message = "Username must be set when Guardian module enabled.";
      }
      {
        assertion = hasAttr cfg.username config.users.users;
        message = "Specified Guardian user '${cfg.username}' does not exist.";
      }
      {
        assertion = config.users.users.${cfg.username}.isNormalUser;
        message = "Specified user '${cfg.username}' must be a normal user.";
      }
    ];

    dotfield.guardian.user = mkAliasDefinitions config.users.users.${cfg.username};
    users.groups."wheel".members = [cfg.username];
    users.users.${cfg.username}.extraGroups =
      [
        "seadome"
        "keys" # sops-nix
      ]
      ++ (optional config.networking.networkmanager.enable "networkmanager")
      ++ (optional config.services.mysql.enable "mysql")
      ++ (optional config.virtualisation.docker.enable "docker");
  };
}
