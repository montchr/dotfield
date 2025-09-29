{
  programs.waybar.settings.primary = {
    layer = "top";
    position = "bottom";
    height = 24;
    # width = 1280;
    # margin = 6;
    spacing = 3;
    modules-left = [
      "niri/workspaces"

      "sway/workspaces"
      "sway/mode"
      "sway/scratchpad"

      "custom/media"
    ];
    modules-center = [
      "niri/window"

      "sway/window"
    ];
    modules-right = [
      "mpd"
      "idle_inhibitor"
      "wireplumber"
      "network"
      "power-profiles-daemon"
      "cpu"
      "memory"
      "temperature"
      "backlight"
      # "keyboard-state"
      "battery"
      "battery#bat2"
      "tray"
      "clock"
      "custom/power"
    ];
    "niri/workspaces" = {
      all-outputs = true;
    };
    "niri/window" = {
      "format" = "{title}";
    };
    "sway/workspaces" = {
      all-outputs = true;
      disable-scroll = true;
      warp-on-scroll = false;
      format = "{name}";
      #      format = "{name} :: {icon}";
      format-icons = {
        "default" = "";
        "urgent" = "";
        "focused" = "";
      };
    };
    backlight = {
      format = "{icon} {percent}%";
      format-icons = [
        "󰪟"
        "󰪡"
        "󰪣"
        "󰪥"
      ];
    };
    battery = {
      format = "{icon} {capacity}%";
      format-alt = "{icon} {time}";
      format-charging = "󰂄 {capacity}%";
      format-full = "{icon} {capacity}%";
      # format-good = "";
      format-icons = [
        "󰁹"
        "󰁻"
        "󰁽"
        "󰁿"
        "󰂁"
      ];
      states = {
        critical = 15;
        warning = 30;
      };
    };
    "battery#bat2" = {
      bat = "BAT2";
    };
    clock = {
      format = "{:%Y-%m-%d %H:%M:%S}";
      format-alt = "{:%d-%m %H:%M}";
      interval = 1;
      on-click-left = "mode";
      # tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
      tooltip-format = "<tt><small>{calendar}</small></tt>";
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
      format = " {}%";
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
    wireplumber = {
      format = "{icon} {volume}% {format_source}";
      format-muted = "󰝟 {format_source}";
      format-bluetooth = "{icon} {volume}% {format_source}";
      format-bluetooth-muted = "{icon} 󰝟 {format_source}";
      format-icons = [
        "󰕿"
        "󰖀"
        "󰕾"
      ];
      scroll-step = 1;
      on-click = "pwvucontrol";
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
      # format-critical = "{temperatureC}°C {icon}";
      format-icons = [
        ""
        ""
        ""
      ];
    };
    tray = {
      spacing = 5;
    };
  };
}
