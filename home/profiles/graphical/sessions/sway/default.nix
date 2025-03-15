{
  flake,
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.wayland.windowManager.sway;
  prefs = import "${flake.self}/users/${config.home.username}/preferences.nix" {
    inherit pkgs;
  };
  mod = cfg.config.modifier;

  swaymsg = "swaymsg";

  screenshotsDir = "$HOME/Pictures/Screenshots";
  screenshotFilename = "${screenshotsDir}/screenshot-$(date '+%Y%m%dT%H%M%S').png";
  screenshot = pkgs.writeShellScript "screenshot.sh" ''
    grim -g "$(slurp -o -r -c '#ff0000ff')" - \
      | satty --filename - --fullscreen \
        --output-filename ${screenshotFilename}
  '';
  screenshotArea = pkgs.writeShellScript "screenshot-area.sh" ''
    mkdir -p "${screenshotsDir}"
    grim -g "$(slurp)" \
    | satty --filename - --output-filename ${screenshotFilename}
  '';

in
{
  imports = [
    ../_wayland-wm.nix

    ./swayosd.nix
  ];

  wayland.windowManager.sway = {
    enable = true;
    # A `null` value tells home-manager to use the package from the
    # system level.
    package = null;
    extraConfigEarly = ''
      # FIXME: this does not help
      # <https://wiki.nixos.org/wiki/Sway#GTK_apps_take_an_exceptionally_long_time_to_start>
      include /etc/sway/config.d/*
    '';
    config = {
      modifier = "Mod4";
      menu = prefs.wayland.menu or "bemenu-run";
      terminal = prefs.term or "foot";
      focus.followMouse = "always";

      bars = [
        { command = prefs.wayland.bar; }
      ];

      window = {
        border = 2;
        titlebar = false;
        commands = [
          {
            command = "floating enable, sticky enable";
            criteria = {
              title = "Picture-in-Picture";
            };
          }
          {
            command = "floating enable, sticky enable";
            criteria = {
              title = ".*Sharing Indicator.*";
            };
          }

        ];
      };

      floating = {
        border = 1;
        titlebar = false;
        criteria = [
          { class = "Pavucontrol"; }
          { app_id = ".*zathura"; }
          { app_id = "mpv"; }
          { app_id = "xdg-desktop-portal-gtk"; }
        ];
      };

      startup = [
        { command = "firefox --profile ~/.mozilla/firefox/home"; }
        { command = "firefox --profile ~/.mozilla/firefox/work"; }
        { command = "emacs"; }
        { command = "waypaper --restore"; }
      ];

      # NOTE: lib.mkOptionDefault is required in order to not wipe out
      # default keybindings!  See the option description.
      keybindings = lib.mkOptionDefault {
        # "${mod}+shift+grave" = "exec emacsclient";

        "${mod}+Shift+e" = ''
          exec swaynag -t warning \
            -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' \
            -B 'Yes, exit sway' \
            'uwsm stop'
        '';
        "${mod}+Ctrl+Alt+Delete" = "uwsm stop";
        "Ctrl+Alt+Delete" = "uwsm stop";
        "${mod}+Ctrl+Alt+Insert" = "exec ${swaymsg} reload";

        "${mod}+Tab" = "workspace next";
        "${mod}+Shift+Tab" = "workspace prev";
        "${mod}+Next" = "workspace next";
        "${mod}+Prior" = "workspace prev";

        "${mod}+F12" = "exec ${screenshot}";
        "${mod}+Shift+F12" = "exec ${screenshotArea}";
        "${mod}+p" = "exec ${screenshot}";
        "${mod}+Shift+p" = "exec ${screenshotArea}";
        "${mod}+Ctrl+p" = "exec kooha";

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

      fonts = {
        names = [
          "Aporetic Sans"
          "Symbols Nerd Font"
        ];
        # style = "Semi-bold";
        size = 10.0;
      };
    };
  };
}
