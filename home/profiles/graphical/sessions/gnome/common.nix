{ lib, ... }:
let
  inherit (lib.hm.gvariant) mkUint32;

  fileChooserDefaults = {
    date-format = "regular";
    location-mode = "path-bar";
    show-hidden = true;
    show-size-column = true;
    show-type-column = true;
    sort-column = "name";
    sort-directories-first = true;
    sort-order = "ascending";
    type-format = "category";
  };
in
{
  imports = [
    ./mimeapps.nix

    ../../__gtk.nix
  ];

  qt.style = "gtk3";
  qt.platformTheme.name = "gtk3";

  dconf.settings = {
    "org/gnome/desktop/interface" = {
      clock-show-seconds = false;
      clock-show-weekday = true;
    };

    "org/gnome/desktop/media-handling" = {
      autorun-never = true;
    };

    "org/gnome/desktop/search-providers" = {
      sort-order = [
        "org.gnome.Nautilus.desktop"
        "org.gnome.Documents.desktop"
      ];
    };

    "org/gnome/desktop/session" = {
      # TODO: longer for home theater, which are the primary gnome users
      idle-delay = (mkUint32 600);
    };

    "org/gnome/shell/app-switcher" = {
      current-workspace-only = false;
    };

    "org/gnome/mutter" = {
      # Modals might get stuck without this setting.
      attach-modal-dialogs = true;
      dynamic-workspaces = true;
      edge-tiling = true;
      # focus-change-on-pointer-rest = true;
      # workspaces-only-on-primary = true;
    };

    "org/gnome/desktop/wm/preferences" = {
      auto-raise = true;
      button-layout = "appmenu:minimize,close";
      focus-mode = "click";
      num-workspaces = 2;
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

    "org/gtk/gtk4/settings/file-chooser" = fileChooserDefaults // { };

    "org/gtk/settings/file-chooser" = fileChooserDefaults // { };
  };
}
