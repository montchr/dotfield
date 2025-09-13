{
  lib,
  self,
  config,
  ...
}:
let
  inherit (config.meta) hosts keys;

in
{
  users.cdom = { };

  meta.users.cdom = {
    whoami = {
      name = "Chris Montgomery";
      firstName = "Chris";
      lastName = "Montgomery";
      email = "chmont@protonmail.com";
      pgp = rec {
        id = "0x135EEDD0F71934F3";
        # TODO: how can this be made a default?
        key = keys.pgp.asc.${id};
      };
      accounts = {
        github = "montchr";
        mastodon = "@montchr@assemblag.es";
        email = {
          personal = {
            primary = true;
            localpart = "chmont";
            domain = "protonmail.com";
            provider = "proton";
          };
          tu = {
            localpart = "tuc26080";
            alias = "chrismont";
            domain = "temple.edu";
            extraAliases = [ ];
            provider = "outlook";
          };
          kleinweb = {
            localpart = "kleinweb";
            shared = true;
            domain = "temple.edu";
            provider = "outlook";
          };
        };
      };
    };

    preferences = rec {
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
      theme.font.families.sansSerif = {
        name = "Inter";
        pname = "inter";
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
        applications = 12;
        desktop = 10;
        popups = 10;
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
        size = 16;
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

    keys.ssh = [
      keys.ssh.cdom-yubikey-rsa
    ]
    ++ hosts.boschic.users.seadoom.keys.ssh
    ++ hosts.brakhage.users.blink.keys.ssh
    ++ hosts.hodgepodge.users.seadoom.keys.ssh
    ++ hosts.ryosuke.users.cdom.keys.ssh
    ++ hosts.tuvix.users.cdom.keys.ssh
    ++ hosts.tuuvok.users.cdom.keys.ssh;
  };
}
