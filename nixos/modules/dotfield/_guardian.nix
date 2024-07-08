{ config, lib, ... }:
let
  inherit (builtins) hasAttr;
  inherit (lib) types;

  cfg = config.dotfield.guardian;
in
{
  options.dotfield.guardian = {
    enable = lib.mkOption {
      default = true;
      type = types.bool;
      description = "Whether to designate a guardian user for this system.";
    };

    username = lib.mkOption {
      type = with types; nullOr str;
      default = null;
      description = ''
        Name of the guardian user. Must be an existing non-system user.
      '';
    };

    extraGroups = lib.mkOption {
      type = with types; listOf str;
      default = [ ];
      description = ''
        Auxilliary groups for the guardian user.
      '';
    };

    keys = {
      all = lib.mkOption {
        type = with types; listOf str;
        default = [ ];
      };
    };
    autoLogin = lib.mkEnableOption "Whether to log the guardian user in automatically.";
  };
  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = hasAttr cfg.username config.users.users;
        message = "Specified Guardian user '${cfg.username}' does not exist.";
      }
      {
        assertion = config.users.users.${cfg.username}.isNormalUser;
        message = "Specified user '${cfg.username}' must be a normal user.";
      }
    ];

    dotfield.guardian.extraGroups =
      (lib.optionals config.services.printing.enable [
        "cups"
        "lp"
      ])
      ++ (lib.optionals config.hardware.sane.enable [
        "scanner"
        "lp"
      ])
      ++ (lib.optional config.networking.networkmanager.enable "networkmanager")
      ++ (lib.optional config.services.keyd.enable "keyd")
      ++ (lib.optional config.services.mysql.enable "mysql")
      ++ (lib.optional config.hardware.openrazer.enable "openrazer")
      ++ (lib.optional config.virtualisation.docker.enable "docker");

    users.groups."wheel".members = [ cfg.username ];
    users.users.${cfg.username}.extraGroups = [
      "seadome"
      "keys" # sops-nix
    ] ++ cfg.extraGroups;
  };
}
