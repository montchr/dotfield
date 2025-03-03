### Sources:
# https://github.com/colemickens/nixcfg/blob/6470ce9f04839d7f152bdc14644f976b8cf5bead/mixins/_preferences.nix
{ pkgs }:
rec {
  editor = "emacsclient";
  term = "ghostty";
  shell = "fish";

  theme.color.variant = "light";
  theme.color.scheme.dark = "catppuccin-mocha";
  theme.color.scheme.light = "catppuccin-latte";
  theme.font.sansSerif = {
    name = "Inter";
    package = pkgs.inter;
  };
  theme.font.serif = {
    name = "Aporetic Serif";
    package = pkgs.aporetic;
  };
  theme.font.monospace = {
    name = "Aporetic Sans Mono";
    package = pkgs.aporetic;
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
    name = "phinger-cursors-dark";
    package = pkgs.phinger-cursors;
    size = 24;
  };
  # theme.gui = {
  #   name = "Arc-Dark";
  #   package = pkgs.arc-theme;
  # };

  wayland = {
    desktop = "sway";
    bar = "waybar";
    launcher = "fuzzel";
    notifications = "dunst";
  };
}
