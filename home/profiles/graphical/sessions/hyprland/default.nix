{
  flake,
  config,
  lib,
  pkgs,
  ...
}:
let
  prefs = import "${flake.self}/users/${config.home.username}/preferences.nix" {
    inherit pkgs;
  };

  launch = type: "${lib.getExe pkgs.handlr-regex} launch ${type}";
  fuzzel = lib.getExe pkgs.fuzzel;
in
{
  imports = [
    ../_wayland-wm.nix

    ./__keybindings.nix
  ];

  # from @misterio77
  # xdg.portal = {
  #   extraPortals = [ pkgs.xdg-desktop-portal-wlr ];
  #   config.hyprland = {
  #     default = [
  #       "wlr"
  #       "gtk"
  #     ];
  #   };
  # };

  home.packages = with pkgs; [
    grimblast
    hypridle
    hyprlock
    hyprpaper
    hyprpicker
    hyprpolkitagent
    hyprsunset
  ];

  wayland.windowManager.hyprland = {
    enable = true;

    # Defer to NixOS module.
    # NOTE: Both of these values must match.
    package = null;
    portalPackage = null;
    # Defer to UWSM.
    systemd.enable = false;

    settings = {
      "$mod" = "SUPER";
      "$terminal" = prefs.term;
      "$fileManager" = prefs.file-manager;
      "$menu" = prefs.wayland.menu;

      general = {
        gaps_in = 8;
        gaps_out = 16;
        border_size = 2;
        resize_on_border = true;
        layout = "dwindle";
      };

      binds = {
        movefocus_cycles_fullscreen = false;
      };

      cursor.inactive_timeout = 4;
      input.touchpad.disable_while_typing = true;

      dwindle = {
        split_width_multiplier = 1.35;
        # pseudotile = true;
      };

      misc = {
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
        rounding = 8;
        rounding_power = 2;
        active_opacity = 1.0;
        inactive_opacity = 0.9;
        fullscreen_opacity = 1.0;
        shadow = {
          enabled = true;
          offset = "3 3";
          range = 12;
          # render_power = 3;
          # color = "0x44000000";
          # color_inactive = "0x66000000";
        };
        blur = {
          enabled = true;
          size = 4;
          passes = 3;
          # TODO: why this number? from default config
          vibrancy = 0.1696;
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

      bind =
        [
          # FIXME: nothing happens!
          "$mod, Return, exec, ${launch "x-scheme-handler/terminal"}"

          "$mod, e, exec, ${launch "text/plain"}"
          "$mod, b, exec, ${launch "x-scheme-handler/https"}"

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

          # Screenshotting
          ",Print, exec, grimblast --notify --freeze copy area"
          "$mod, F12, exec, grimblast --notify --freeze copy output"
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
}
