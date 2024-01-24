{
  config,
  lib,
  ...
}: let
  inherit (builtins) hasAttr;

  cfg = config.dotfield.guardian;
in {
  options.dotfield.guardian = {
    enable = lib.mkEnableOption "Whether to designate a guardian user for this system.";
    username = lib.mkOption {
      type = with lib.types; nullOr str;
      default = null;
      description = ''
        Name of the guardian user. Must be an existing non-system user.
      '';
    };
    user = lib.mkOption {
      readOnly = true;
    };
    keys = {
      all = lib.mkOption {
        type = with lib.types; listOf str;
        default = [];
      };
    };
    autoLogin = lib.mkEnableOption "Whether to log the guardian user in automatically.";
  };
  config = lib.mkIf cfg.enable {
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

    dotfield.guardian.user = lib.mkAliasDefinitions config.users.users.${cfg.username};
    users.groups."wheel".members = [cfg.username];
    users.users.${cfg.username}.extraGroups =
      [
        "seadome"
        "keys" # sops-nix
      ]
      ++ (lib.optional config.networking.networkmanager.enable "networkmanager")
      ++ (lib.optional config.services.keyd.enable "keyd")
      ++ (lib.optional config.services.mysql.enable "mysql")
      ++ (lib.optional config.virtualisation.docker.enable "docker");
  };
}
