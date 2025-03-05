{ lib, config, ... }:
let
  swaycfg = config.wayland.windowManager.sway;
  withSway = items: lib.optionals swaycfg.enable items;
in
{
  programs.waybar.settings.primary = {
    layer = "top";
    position = "bottom";
    height = 16;
    # width = 1280;
    spacing = 3;
    modules-left =
      [ ]
      ++ (withSway [
        "sway/workspaces"
        "sway/mode"
        "sway/scratchpad"
      ])
      ++ [ "custom/media" ];
    modules-center = [ ] ++ (withSway [ "sway/window" ]);
    modules-right =
      [
        "mpd"
        "idle_inhibitor"
        "pulseaudio"
        "network"
        "power-profiles-daemon"
        "cpu"
        "memory"
        "temperature"
        "backlight"
        # "keyboard-state"
      ]
      ++ (withSway [
        #  "sway/language"
      ])
      ++ [
        "battery"
        "battery#bat2"
        "clock"
        "tray"
        "custom/power"
      ];
    "sway/workspaces" = {
      all-outputs = true;
      disable-scroll = true;
      warp-on-scroll = false;
      format = "{name} :: {icon}";
      format-icons = {
        "default" = "";
        "urgent" = "";
        "focused" = "";
      };
    };
    backlight = {
      format = "{icon} {percent}%";
      format-icons = [
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
      ];
    };
    battery = {
      format = "{icon} {capacity}%";
      format-alt = "{icon} {time}";
      format-charging = "󰂄 {capacity}%";
      format-full = "{icon} {capacity}%";
      format-good = "";
      format-icons = [
        ""
        ""
        ""
        ""
        ""
      ];
      format-plugged = " {capacity}%";
      states = {
        critical = 15;
        good = 95;
        warning = 30;
      };
    };
    "battery#bat2" = {
      bat = "BAT2";
    };
    clock = {
      format = "{:%Y-%m-%d %H:%M:%S %z}";
      format-alt = "{:%m/%d %H:%M:%S}";
      interval = 1;
      on-click-left = "mode";
      tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
    };
    cpu = {
      format = " {usage}%";
      tooltip = false;
    };
    "custom/media" = {
      escape = true;
      exec = "$HOME/.config/waybar/mediaplayer.py --player spotify 2> /dev/null";
      format = "{icon} {text}";
      format-icons = {
        default = "🎜";
        spotify = "";
      };
      max-length = 40;
      return-type = "json";
    };
    "custom/power" = {
      format = "⏻ ";
      menu = "on-click";
      menu-actions = {
        hibernate = "systemctl hibernate";
        reboot = "reboot";
        shutdown = "shutdown";
        suspend = "systemctl suspend";
      };
      menu-file = "$HOME/.config/waybar/power_menu.xml";
      tooltip = false;
    };
    idle_inhibitor = {
      format = "{icon}";
      format-icons = {
        activated = "";
        deactivated = "";
      };
    };
    keyboard-state = {
      capslock = true;
      format = "{name} {icon}";
      format-icons = {
        locked = "";
        unlocked = "";
      };
      numlock = true;
    };
    memory = {
      format = " {}%";
    };
    mpd = {
      consume-icons = {
        on = " ";
      };
      format = "{stateIcon} {consumeIcon}{randomIcon}{repeatIcon}{singleIcon}{artist} - {album} - {title} ({elapsedTime:%M:%S}/{totalTime:%M:%S}) ⸨{songPosition}|{queueLength}⸩ {volume}% ";
      format-disconnected = "󰝛";
      format-stopped = "󰝚  {consumeIcon}{randomIcon}{repeatIcon}{singleIcon}";
      interval = 5;
      random-icons = {
        off = "<span color=\"#f53c3c\"></span> ";
        on = " ";
      };
      repeat-icons = {
        on = " ";
      };
      single-icons = {
        on = "󰑘 ";
      };
      state-icons = {
        paused = "";
        playing = "";
      };
      tooltip-format = "MPD (connected)";
      tooltip-format-disconnected = "MPD (disconnected)";
      unknown-tag = "N/A";
    };
    network = {
      format-alt = "{ifname}: {ipaddr}/{cidr}";
      format-disconnected = "󰌙 Disconnected";
      format-ethernet = "󰈀 {ipaddr}/{cidr}";
      format-linked = "󰌚 {ifname} (No IP)";
      format-wifi = "  {essid} ({signalStrength}%)";
      tooltip-format = "󰌘 {ifname} via {gwaddr}";
    };
    power-profiles-daemon = {
      format = "{icon}";
      format-icons = {
        balanced = "䷎";
        default = "䷯";
        performance = "䷏";
        power-saver = "䷟";
      };
      tooltip = true;
      tooltip-format = "Power profile: {profile}\nDriver: {driver}";
    };
    pulseaudio = {
      format = "{icon} {volume}% {format_source}";
      format-bluetooth = "{icon} {volume}% {format_source}";
      format-bluetooth-muted = "󰝟 {icon} {format_source}";
      format-icons = {
        car = "󰄋";
        default = [
          "󰕿"
          "󰖀"
          "󰕾"
        ];
        hands-free = "󰗋";
        headphone = "󰋋";
        headset = "󰋎";
        phone = "󰏲";
        portable = "󰄜";
      };
      format-muted = "󰝟 {format_source}";
      format-source = " {volume}%";
      format-source-muted = "";
      on-click = "pwvucontrol";
      scroll-step = 1;
    };
    "sway/mode" = {
      format = "<span style=\"italic\">{}</span>";
    };
    "sway/scratchpad" = {
      format = "{icon} {count}";
      format-icons = [
        ""
        ""
      ];
      show-empty = false;
      tooltip = true;
      tooltip-format = "{app}: {title}";
    };
    temperature = {
      critical-threshold = 80;
      format = "{icon} {temperatureC}°C";
      format-critical = "{temperatureC}°C {icon}";
      format-icons = [
        ""
        ""
        ""
      ];
    };
    tray = {
      spacing = 10;
    };
  };
}
