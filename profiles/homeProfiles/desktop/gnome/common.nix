{lib, ...}:
with lib.hm.gvariant; {
  imports = [../gtk/dconf-settings.nix];

  dconf.settings = {
    "org/gnome/desktop/session" = {
      # TODO: longer for home theater, which are the primary gnome users
      idle-delay = lib.mkDefault mkUint32 600;
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
