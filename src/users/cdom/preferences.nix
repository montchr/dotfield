{
  meta.users.cdom.preferences = rec {
    editor = "emacsclient";
    term = "ghostty";
    shell = "fish";
    file-manager = "nemo";
    # audio-player = "mpv";
    pdf-reader = "zathura";
    video-player = "mpv";
    web-browser = "firefox";

    theme.color.variant = "dark";
    # theme.color.variant = "light";
    # theme.color.scheme.dark = "catppuccin-frappe";
    theme.color.scheme.dark = "catppuccin-mocha";
    # theme.color.scheme.dark = "catppuccin-macchiato";
    # theme.color.scheme.dark = "black-metal-khold";
    theme.color.scheme.light = "catppuccin-latte";
    # theme.font.families.sansSerif = {
    #   name = "Inter";
    #   pname = "inter";
    # };
    theme.font.families.sansSerif = {
      name = "Berkeley Mono";
      pname = "berkeley-mono";
    };
    theme.font.families.serif = {
      name = "Aporetic Serif";
      pname = "aporetic";
    };
    theme.font.families.monospace = {
      name = "Aporetic Sans Mono";
      pname = "aporetic";
    };
    theme.font.sizes = {
      applications = 10;
      desktop = 10;
      popups = 8;
      terminal = 10;
    };
    theme.icons = {
      pname = "papirus-icon-theme";
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
      pname = "bibata-cursors";
      size = 10;
    };
    theme.gui = {
      pname = "flat-remix-gtk";
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
  };
}
