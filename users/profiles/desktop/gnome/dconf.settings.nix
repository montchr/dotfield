# Adjusted manually from generated output of dconf2nix
# https://github.com/gvolpe/dconf2nix

{ lib, pkgs, ... }:

with lib.hm.gvariant;

{
  dconf.settings = lib.mkIf (!pkgs.stdenv.hostPlatform.isDarwin) {
    # "org/gnome/Disks" = {
    #   image-dir-uri = "file:///home/seadoom/Documents";
    # };

    # "org/gnome/baobab/ui" = {
    #   window-size = mkTuple [ 960 600 ];
    #   window-state = 87168;
    # };

    "org/gnome/calculator" = {
      accuracy = 9;
      angle-units = "degrees";
      base = 10;
      button-mode = "basic";
      number-format = "automatic";
      show-thousands = false;
      show-zeroes = true;
      # TODO: EUR
      source-currency = "";
      # source-units = "degree";
      # TODO: USD
      target-currency = "";
      # target-units = "radian";
      # window-position = mkTuple [ 26 23 ];
      word-size = 64;
    };

    # FIXME: only for HodgePodge
    # "org/gnome/cheese" = {
    #   burst-delay = 1000;
    #   camera = "Apple Facetime HD";
    #   photo-x-resolution = 1280;
    #   photo-y-resolution = 720;
    #   video-x-resolution = 1280;
    #   video-y-resolution = 720;
    # };

    "org/gnome/desktop/input-sources" = {
      per-window = false;
      sources = [ (mkTuple [ "xkb" "us" ]) ];
      xkb-options = [ "terminate:ctrl_alt_bksp" "caps:ctrl_modifier" ];
    };

    "org/gnome/desktop/peripherals/touchpad" = {
      natural-scroll = false;
      tap-to-click = true;
      two-finger-scrolling-enabled = true;
    };

    "org/gnome/desktop/privacy" = {
      # FIXME: per-host opt-in
      # disable-microphone = false;
    };

    "org/gnome/desktop/search-providers" = {
      # sort-order = [ "org.gnome.Contacts.desktop" "org.gnome.Documents.desktop" "org.gnome.Nautilus.desktop" ];
    };

    "org/gnome/desktop/wm/preferences" = {
      workspace-names = [ "sys" "talk" "web" "edit" "run" ];
    };

    "org/gnome/mutter" = {
      attach-modal-dialogs = true;
      dynamic-workspaces = true;
      edge-tiling = true;
      focus-change-on-pointer-rest = true;
      # FIXME: probably not desirable
      workspaces-only-on-primary = true;
    };

    "org/gnome/shell" = {
      disabled-extensions = [
        "window-list@gnome-shell-extensions.gcampax.github.com"
        "native-window-placement@gnome-shell-extensions.gcampax.github.com"
        "workspace-indicator@gnome-shell-extensions.gcampax.github.com"
      ];
      enabled-extensions = [
        "auto-move-windows@gnome-shell-extensions.gcampax.github.com"
        "user-theme@gnome-shell-extensions.gcampax.github.com"
        "windowsNavigator@gnome-shell-extensions.gcampax.github.com"
      ];
      favorite-apps = [
        "firefox.desktop"
        "kitty.desktop"
        "emacsclient.desktop"
        "org.gnome.Nautilus.desktop"
      ];
      remember-mount-password = false;
    };

    "org/gnome/shell/extensions/auto-move-windows" = {
      application-list = [
        ##: 01: 'sys'
        "htop.desktop:1"

        ##: 02: 'talk'
        # TODO

        ##: 03: 'web'
        "firefox.desktop:3"

        ##: 04: 'edit'
        "emacsclient.desktop:4"

        ##: 05: 'run'
        "kitty.desktop:5"
      ];
    };

    "org/gnome/shell/extensions/user-theme" = {
      # FIXME
      name = "";
    };

    # "org/gnome/shell/world-clocks" = {
    #   locations = "@av []";
    # };

    # "org/gtk/settings/color-chooser" = {
    #   selected-color = mkTuple [ true 0.8705882352941177 ];
    # };

    "org/gtk/settings/file-chooser" = {
      # FIXME: which format?
      date-format = "regular";
      location-mode = "path-bar";
      show-hidden = true;
      show-size-column = true;
      show-type-column = true;
      # sidebar-width = 157;
      sort-column = "name";
      sort-directories-first = true;
      sort-order = "ascending";
      type-format = "category";
      # window-position = mkTuple [ 26 23 ];
      # window-size = mkTuple [ 1203 821 ];
    };

  };
}
