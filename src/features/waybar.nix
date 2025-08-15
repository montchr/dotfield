{ lib, ... }:
{
  dotfield.features.waybar.home =
    { config, pkgs, ... }:
    {
      programs.waybar.enable = true;
      programs.waybar.systemd.enable = true;
      programs.waybar.systemd.target = "tray.target";

      home.packages = [ config.programs.waybar.package ];

      systemd.user.services.waybar = {
        Unit = {
          # BindsTo = [ "tray.target" ];
          After = lib.mkForce [ "graphical-session.target" ];
          Requisite = [ "graphical-session.target" ];
          X-Reload-Triggers = [
            "${config.xdg.configHome}/waybar/config"
            "${config.xdg.configHome}/waybar/style.css"
            "${config.xdg.configHome}/waybar/custom.css"
          ];
        };
        Service = {
          ExecStartPost = "${pkgs.coreutils}/bin/sleep 2";
        };
      };
    };
}
