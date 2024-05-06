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

    ../../gtk.nix
  ];

  # The package loaded by the "gnome" setting is obsolete.
  # TODO: gtk4?
  qt.platformTheme.name = "gtk3";

  services.gnome-keyring.enable = true;

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
      idle-delay = lib.mkDefault (mkUint32 600);
    };

    "org/gnome/shell/app-switcher" = {
      current-workspace-only = false;
    };

    "org/gnome/mutter" = {
      # Prevent annoying inability to exit eternal recurrence of this modal existence.
      attach-modal-dialogs = true;
      dynamic-workspaces = true;
      edge-tiling = true;
      # focus-change-on-pointer-rest = true;
      # workspaces-only-on-primary = true;
    };

    "org/gnome/desktop/wm/preferences" = {
      auto-raise = true;
      # FIXME: want maximimize/toggle
      button-layout = "appmenu:minimize,close";
      focus-mode = "click";
      num-workspaces = 2;
      # FIXME: no hardcode
      # titlebar-font = "Iosevka Comfy 10";
      # workspace-names = [ "sys" "talk" "web" "edit" "run" ];
    };

    "org/gnome/desktop/privacy" = {
      disable-camera = true;
      disable-microphone = true;
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
