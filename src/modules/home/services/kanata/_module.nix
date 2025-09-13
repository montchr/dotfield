{
  pkgs,
  config,
  lib,
  ...
}:
let
  inherit (lib)
    mkEnableOption
    mkOption
    mkPackageOption
    types
    ;
  cfg = config.services.kanata;
in
{
  options.services.kanata = {
    enable = mkEnableOption "Kanata keyboard remapping daemon";
    package = mkPackageOption pkgs "kanata" {
      example = [ "kanata-with-cmd" ];
      extraDescription = ''
        ::: {.note}
        If {option}`danger-enable-cmd` is enabled in any of the keyboards, the
        `kanata-with-cmd` package should be used.
        :::
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];

    systemd.user.services.kanata = {
      Unit = {
        Description = "Keyboard remapping daemon";
      };
      Service = {
        ExecStart = "${lib.getExe cfg.package}";
        Restart = "on-failure";
        RestartSec = 2;
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
    };
  };
}
