{
  users.cdom.aspects.desktop-sessions__wayland-wm.home =
    {
      pkgs,
      lib,
      config,
      ...
    }:
    let
      cfg = config.programs.waybar;
    in
    {
      imports = [ ./__primary.nix ];

      stylix.targets.waybar.addCss = false;

      programs.waybar.enable = true;
      programs.waybar.systemd.enable = true;
      programs.waybar.systemd.target = "tray.target";
      programs.waybar.style = ''
        @import "./custom.css";
      '';

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

      home.packages = [ cfg.package ];
    };
}
