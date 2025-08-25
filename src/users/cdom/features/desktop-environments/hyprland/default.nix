flake@{ self, lib, ... }:
{
  dotfield.users.cdom.aspects.hyprland.home =
    { config, pkgs, ... }:
    let
      prefs = flake.config.dotfield.meta.users.cdom.preferences;
      launch = type: "${lib.getExe pkgs.handlr-regex} launch ${type}";
      fuzzel = lib.getExe pkgs.fuzzel;
    in
    {
      imports = [
        ./__keybindings.nix
      ];

      # systemd.user.services."wayland-wm@Hyprland" = {
      #   Service.TimeoutStartSec = 30;
      # };

      wayland.windowManager.hyprland = {
        enable = true;

        # Defer to NixOS module.
        # NOTE: Both of these values must match.
        package = null;
        portalPackage = null;

        systemd = {
          # Defer to UWSM.
          enable = false;
          # variables = [ "--all" ];
          # extraCommands = [
          #   "systemctl --user stop graphical-session.target"
          #   "systemctl --user start hyprland-session.target"
          # ];
        };

        settings = {
          "$mod" = "SUPER";
          "$terminal" = launch "x-scheme-handler/terminal";
          "$browser" = launch "x-scheme-handler/https";
          "$fileManager" = prefs.file-manager;
          "$menu" = prefs.wayland.menu;

          debug = {
            disable_logs = false;
            enable_stdout_logs = true;
          };

          general = {
            gaps_in = 6;
            gaps_out = 12;
            border_size = 2;
            resize_on_border = true;
            layout = "dwindle";
          };

          binds = {
            movefocus_cycles_fullscreen = false;
          };

          cursor.inactive_timeout = 4;

          input.touchpad.disable_while_typing = true;
          input.touchpad.clickfinger_behavior = true;

          dwindle = {
            split_width_multiplier = 1.35;
            # pseudotile = true;
          };

          misc = {
            # No, Hyprland community, I absolutely do not want to see your
            # creepy and unsolicited anime girl background.  This opt-out
            # setting is proof that Drew DeVault was right about you.
            disable_hyprland_logo = true;
            focus_on_activate = true;
            # Unfullscreen when opening something
            new_window_takes_over_fullscreen = 2;
          };

          layerrule = [
            "animation fade,hyprpicker"
            "animation fade,selection"

            "animation fade, ${prefs.wayland.bar}"
            "blur, ${prefs.wayland.bar}"
            "ignorezero, ${prefs.wayland.bar}"

            "blur, notifications"
            "ignorezero, notifications"

            "blur, ${prefs.wayland.menu}"
            "ignorezero, ${prefs.wayland.menu}"

            "noanim,wallpaper"
          ];

          decoration = {
            rounding = 6;
            rounding_power = 2;
            active_opacity = 1.0;
            inactive_opacity = 0.95;
            fullscreen_opacity = 1.0;
            shadow = {
              enabled = true;
              # offset = "3 3";
              # range = 12;
              # render_power = 3;
            };
            blur = {
              enabled = true;
              size = 4;
              passes = 3;
              new_optimizations = true;
              ignore_opacity = true;
              popups = true;
            };
          };

          animations = {
            enabled = true;
            bezier = [
              "easein, 0.1, 0, 0.5, 0"
              "easeinback, 0.35, 0, 0.95, -0.3"

              "easeout, 0.5, 1, 0.9, 1"
              "easeoutback, 0.35, 1.35, 0.65, 1"

              "easeinout, 0.45, 0, 0.55, 1"
            ];
            animation = [
              "fadeIn, 1, 3, easeout"
              "fadeLayersIn, 1, 3, easeoutback"
              "layersIn, 1, 3, easeoutback, slide"
              "windowsIn, 1, 3, easeoutback, slide"

              "fadeLayersOut, 1, 3, easeinback"
              "fadeOut, 1, 3, easein"
              "layersOut, 1, 3, easeinback, slide"
              "windowsOut, 1, 3, easeinback, slide"

              "border, 1, 3, easeout"
              "fadeDim, 1, 3, easeinout"
              "fadeShadow, 1, 3, easeinout"
              "fadeSwitch, 1, 3, easeinout"
              "windowsMove, 1, 3, easeoutback"
              "workspaces, 1, 2.6, easeoutback, slide"
            ];
          };

          # exec = [
          # ];

          exec-once = [
            "exec uwsm finalize"
          ];

          bind = [
            "$mod, Return, exec, $terminal"

            # FIXME: nothing happens!
            "$mod, e, exec, ${launch "text/plain"}"

            # Open default web browser
            "$mod, b, exec, $browser"

            "$mod, d, exec, $menu"

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

            # Screenshots
            ",Print, exec, grimblast --notify --freeze copy area"
            "$mod, F12, exec, grimblast --notify --freeze copy area"
            "SHIFT, Print, exec, grimblast --notify --freeze copy output"
            "$mod SHIFT, F12, exec, grimblast --notify --freeze copy output"
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
          )
          ++
            # Screen lock
            (
              let
                swaylock = lib.getExe config.programs.swaylock.package;
              in
              lib.optionals config.programs.swaylock.enable [
                "SUPER, backspace, exec, ${swaylock} -S --grace 2 --grace-no-mouse"
                "SUPER, XF86Calculator, exec, ${swaylock} -S --grace 2 --grace-no-mouse"
              ]
            )
          ++
            # Notification manager
            (
              let
                makoctl = lib.getExe' config.services.mako.package "makoctl";
              in
              lib.optionals config.services.mako.enable [
                "SUPER, w, exec, ${makoctl} dismiss"
                "SUPERSHIFT, w, exec, ${makoctl} restore"
              ]
            )
          ++
            # Clipboard manager
            (
              let
                cliphist = lib.getExe config.services.cliphist.package;
              in
              lib.optionals config.services.cliphist.enable [
                ''SUPER, c, exec, selected=$(${cliphist} list | ${fuzzel} -d) && echo "$selected" | ${cliphist} decode | wl-copy''
              ]
            );
        };
      };
    };
}
