{lib, ...}:
with lib.hm.gvariant; {
  imports = [../gtk/dconf-settings.nix];

  dconf.settings = {
    "org/gnome/desktop/interface" = {
      clock-show-seconds = lib.mkDefault false;
      clock-show-weekday = lib.mkDefault true;

      # FIXME: too aggressive, need CUA-like also -- macOS keys are actually
      # pretty nice as a weird combination of Emacs and CUA with cmd/ctrl swap
      # Emacs because it's so ingrained by both shells and even macOS defaults.
      # But it's understandable why it's not the default:
      # e.g. "C-a" is no longer for select all -- macOS can get away with this
      # because it doesn't use Ctrl as prominently as Super
      gtk-key-theme = "Emacs";

      # text-scaling-factor = 1.0;
      # toolkit-accessibility = false;
    };

    "org/gnome/desktop/session" = {
      # TODO: longer for home theater, which are the primary gnome users
      idle-delay = lib.mkDefault (mkUint32 600);
    };

    "org/gnome/desktop/privacy" = {
      old-files-age = mkUint32 30;
      recent-files-max-age = -1;
    };

    "org/gnome/desktop/notifications/application/firefox" = {
      application-id = "firefox.desktop";
    };

    "org/gnome/desktop/notifications/application/signal-desktop" = {
      application-id = "signal-desktop.desktop";
    };

    "org/gnome/desktop/notifications/application/teams" = {
      application-id = "teams.desktop";
    };

    "org/gnome/desktop/peripherals/keyboard" = {
      repeat = true;
    };
  };
}
