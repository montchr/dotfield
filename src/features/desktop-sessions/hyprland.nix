flake@{ lib, ... }:
{
  aspects.desktop-sessions__hyprland = {
    requires = [ "desktop-sessions__wayland-wm" ];

    nixos = {
      programs.hyprland.withUWSM = true;
      programs.uwsm.waylandCompositors.hyprland = {
        prettyName = "Hyprland";
        comment = "Hyprland compositor managed by UWSM";
        binPath = "/run/current-system/sw/bin/Hyprland";
      };

      programs.hyprland.enable = true;
      programs.hyprland.xwayland.enable = true;
    };

    home =
      { config, pkgs, ... }:
      let
        prefs = flake.config.meta.users.${config.home.username}.preferences;
      in
      {
        wayland.windowManager.hyprland = {
          enable = true;

          # Defer to NixOS module.
          # NOTE: Both of these values must match.
          package = null;
          portalPackage = null;

          # Defer to UWSM.
          systemd.enable = false;

          settings = {
            "$fileManager" = prefs.file-manager;
            "$menu" = prefs.wayland.menu;
            misc.disable_hyprland_logo = true;
            exec-once = lib.mkAfter [
              "exec uwsm finalize"
            ];
            bind = [
              # Brightness
              ",XF86MonBrightnessUp, exec, brightnessctl set 5%+"
              ",XF86MonBrightnessDown, exec, brightnessctl set 5%-"
              "SHIFT, XF86MonBrightnessUp, exec, brightnessctl -d kbd_backlight set 10%+"
              "SHIFT, XF86MonBrightnessDown, exec, brightnessctl -d kbd_backlight set 10%-"

              # Volume
              ",XF86AudioRaiseVolume, exec, pactl set-sink-volume @DEFAULT_SINK@ +5%"
              ",XF86AudioLowerVolume, exec, pactl set-sink-volume @DEFAULT_SINK@ -5%"
              ",XF86AudioMute, exec, pactl set-sink-mute @DEFAULT_SINK@ toggle"
              "SHIFT, XF86AudioRaiseVolume, exec, pactl set-source-volume @DEFAULT_SOURCE@ +5%"
              "SHIFT, XF86AudioLowerVolume, exec, pactl set-source-volume @DEFAULT_SOURCE@ -5%"
              "SHIFT, XF86AudioMute, exec, pactl set-source-mute @DEFAULT_SOURCE@ toggle"
              ",XF86AudioMicMute, exec, pactl set-source-mute @DEFAULT_SOURCE@ toggle"
            ]
            ++ (
              let
                playerctl = lib.getExe' config.services.playerctld.package "playerctl";
                playerctld = lib.getExe' config.services.playerctld.package "playerctld";
              in
              lib.optionals config.services.playerctld.enable [
                # Media control
                ",XF86AudioNext, exec, ${playerctl} next"
                ",XF86AudioPrev, exec, ${playerctl} previous"
                ",XF86AudioPlay, exec, ${playerctl} play-pause"
                ",XF86AudioStop, exec, ${playerctl} stop"
                "SHIFT, XF86AudioNext, exec, ${playerctld} shift"
                "SHIFT, XF86AudioPrev, exec, ${playerctld} unshift"
                "SHIFT, XF86AudioPlay, exec, systemctl --user restart playerctld"
              ]
            );
          };
        };

        home.packages = with pkgs; [
          grimblast
          hypridle
          hyprlock
          hyprpaper
          hyprpicker
          hyprpolkitagent
          hyprsunset
        ];

      };
  };
}
