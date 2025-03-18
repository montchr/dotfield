### Sources:
# https://github.com/colemickens/nixcfg/blob/6470ce9f04839d7f152bdc14644f976b8cf5bead/mixins/_preferences.nix
{ pkgs }:
rec {
  editor = "emacsclient";
  term = "ghostty";
  shell = "fish";
  file-manager = "nemo";
  audio-player = "mpv";
  video-player = "mpv";
  web-browser = "firefox";

  # theme.color.variant = "dark";
  theme.color.variant = "light";
  # theme.color.scheme.dark = "catppuccin-frappe";
  theme.color.scheme.dark = "catppuccin-mocha";
  # theme.color.scheme.dark = "catppuccin-macchiato";
  # theme.color.scheme.dark = "black-metal-khold";
  theme.color.scheme.light = "catppuccin-latte";
  theme.font.families.sansSerif = {
    name = "Inter";
    package = pkgs.inter;
  };
  theme.font.families.serif = {
    name = "Aporetic Serif";
    package = pkgs.aporetic;
  };
  theme.font.families.monospace = {
    name = "Aporetic Sans Mono";
    package = pkgs.aporetic;
  };
  theme.font.sizes = {
    applications = 12;
    desktop = 10;
    popups = 10;
    terminal = 10;
  };
  theme.icons = {
    package = pkgs.papirus-icon-theme;
    dark = "Papirus Dark";
    light = "Papirus Light";
  };
  theme.wallpaper = {
    # image = "";
    mode = "fit";
  };
  # alternatively: posy-cursors / graphite-cursors / vanilla-dmz /
  # catppuccin-cursors / hackneyed-x11-cursors / openzone-cursors
  theme.cursor = {
    # name = "phinger-cursors-dark";
    # package = pkgs.phinger-cursors;
    # size = 24;
    # name = "Posy_Cursor_Black";
    # package = pkgs.posy-cursors;
    # size = 32;
    name = "Bibata-Modern-Classic";
    package = pkgs.bibata-cursors;
    size = 16;
  };
  theme.gui = {
    package = pkgs.flat-remix-gtk;
    name =
      if (theme.color.variant == "light") then
        "Flat-Remix-GTK-Grey-Light"
      else
        "Flat-Remix-GTK-Grey-Darkest";
  };

  wayland = {
    desktop = "hyprland";
    bar = "waybar";
    menu = "fuzzel";
    launcher = "fuzzel";
    notifications = "mako";
  };
}
