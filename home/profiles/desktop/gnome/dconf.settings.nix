# Adjusted manually from generated output of dconf2nix
# https://github.com/gvolpe/dconf2nix
{
  lib,
  pkgs,
  ...
}:
with lib.hm.gvariant; {
  dconf.settings = lib.mkIf (!pkgs.stdenv.hostPlatform.isDarwin) {
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

    # "org/gnome/desktop/input-sources" = {
    #   per-window = false;
    #   sources = [(mkTuple ["xkb" "us"])];
    #   xkb-options = ["terminate:ctrl_alt_bksp" "caps:ctrl_modifier"];
    # };

    "org/gnome/desktop/peripherals/touchpad" = {
      natural-scroll = false;
      tap-to-click = true;
      two-finger-scrolling-enabled = true;
    };

    "org/gnome/desktop/wm/preferences" = {
      workspace-names = ["sys" "talk" "web" "edit" "run"];
    };

    "org/gnome/mutter" = {
      edge-tiling = true;
      workspaces-only-on-primary = false;
    };

    "org/gnome/shell" = {
      disabled-extensions = [
        "window-list@gnome-shell-extensions.gcampax.github.com"
        "native-window-placement@gnome-shell-extensions.gcampax.github.com"
        "workspace-indicator@gnome-shell-extensions.gcampax.github.com"
        "auto-move-windows@gnome-shell-extensions.gcampax.github.com"
        "windowsNavigator@gnome-shell-extensions.gcampax.github.com"
      ];
      enabled-extensions = [
        "user-theme@gnome-shell-extensions.gcampax.github.com"
      ];
      favorite-apps = [
        "firefox.desktop"
        "kitty.desktop"
        "emacs.desktop"
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
        "emacs.desktop:4"
        "emacsclient.desktop:4"

        ##: 05: 'run'
        "kitty.desktop:5"
      ];
    };

    # "org/gnome/shell/extensions/user-theme" = {
    #   # FIXME
    #   name = "";
    # };

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
      sort-column = "name";
      sort-directories-first = true;
      sort-order = "ascending";
      type-format = "category";
    };
  };
}
