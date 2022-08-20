{
  options,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.types) nullOr str submodule;
  inherit (pkgs.lib.our) mkOpt mkOpt' mkBoolOpt;

  cfg = config.dotfield.guardian;
in {
  options.dotfield.guardian = {
    enable = lib.mkEnableOption "Whether to designate a guardian user for this system.";
    username = lib.mkOption {
      type = nullOr str;
      default = null;
      # FIXME: validate that this is an existing user.
      description = ''
        Name of the guardian user. Must be an existing non-system user.
      '';
    };
    user = lib.mkOption {
      readOnly = true;
    };
    autoLogin = lib.mkEnableOption "Whether to log the guardian user in automatically.";
  };
  config = lib.mkIf cfg.enable {
    dotfield.guardian.user = lib.mkAliasDefinitions config.users.users.${cfg.username};
    users.groups."wheel".members = [cfg.username];
  };
}
