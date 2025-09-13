{ lib' }:
{
  pkgs,
  config,
  lib,
  ...
}:
let
  inherit (lib') isEmpty;
  inherit (lib) mkEnableOption mkOption types;
  bashCfg = config.programs.bash;
  cfg = config.programs.bash.blesh;
in
{
  options.programs.bash.blesh = {
    enable = mkEnableOption "ble.sh";
    configFile = mkOption {
      type = types.nullOr (
        types.pathWith {
          absolute = null;
          inStore = null;
        }
      );
      default = null;
      description = "Path to the ble.sh user config file.";
    };
  };

  config = lib.mkIf (bashCfg.enable && cfg.enable) {
    home.packages = [ pkgs.blesh ];

    programs.bash.bashrcExtra = lib.mkBefore ''
      [[ $- == *i* ]] && source -- ${pkgs.blesh}/share/blesh/ble.sh --attach=none \
        ${lib.optionalString (!isEmpty cfg.configFile) "--rcfile ${cfg.configFile}"}
    '';

    programs.bash.initExtra = lib.mkAfter ''
      [[ ! ''${BLE_VERSION-} ]] || ble-attach
    '';
  };
}
