moduleArgs@{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.wayland.windowManager.sway;
  theme = config.theme;
  fxCfg = config.programs.firefox;
  mod = cfg.config.modifier;
in
{
  imports = [
    ../../__wlroots.nix
  ];

  wayland.windowManager.sway = {
    enable = true;
    # A `null` value tells home-manager to use the package from the
    # system level.
    package = null;
    extraConfigEarly = ''
      # <https://wiki.nixos.org/wiki/Sway#GTK_apps_take_an_exceptionally_long_time_to_start>
      include /etc/sway/config.d/*
    '';
    config = {
      modifier = "Mod4";
      terminal = lib.mkDefault "foot";
      startup = [
        {
          command = "firefox --profile ~/.mozilla/firefox/home";
        }
        { command = "emacs"; }
        { command = "waypaper --restore"; }
      ];
      # NOTE: lib.mkOptionDefault is required in order to not wipe out
      # default keybindings!  See the option description.
      keybindings = lib.mkOptionDefault {
        # "$mod+shift+`" = "exec emacsclient";

        # <https://wiki.archlinux.org/title/Sway#Custom_keybindings>
        "XF86MonBrightnessDown" = "exec brightnessctl set 5%-";
        "XF86MonBrightnessUp" = "exec brightnessctl set 5%+";
        "shift+XF86MonBrightnessDown" = "exec brightnessctl -d kbd_backlight set 10%-";
        "shift+XF86MonBrightnessUp" = "exec brightnessctl -d kbd_backlight set 10%+";
        "XF86Search" = "exec fuzzel";
        "XF86AudioMicMute" = "exec pactl set-source-mute @DEFAULT_SOURCE@ toggle";
        "XF86AudioPrev" = "exec playerctl previous";
        "XF86AudioPlay" = "exec playerctl play-pause";
        "XF86AudioNext" = "exec playerctl next";
        "XF86AudioMute" = "exec pactl set-sink-mute @DEFAULT_SINK@ toggle";
        "XF86AudioLowerVolume" = "exec pactl set-sink-volume @DEFAULT_SINK@ -5%";
        "XF86AudioRaiseVolume" = "exec pactl set-sink-volume @DEFAULT_SINK@ +5%";
      };
      # output = {};
      seat = {
        "*" = {
          hide_cursor = "when-typing enable";
        };
      };
      floating.criteria = [
        { class = "Pavucontrol"; }
      ];

      fonts = {
        names = [
          "Aporetic Serif"
          "Symbols Nerd Font"
        ];
        # style = "Semi-bold";
        size = 10.0;
      };
    };
  };
}
