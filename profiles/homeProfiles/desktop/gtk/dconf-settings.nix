# TODO: rename since this is also gtk stuff
{
  lib,
  config,
  ...
}: let
  inherit (config) theme;
in
  with lib.hm.gvariant; {
    dconf.settings = {
      "org/gnome/desktop/interface" = {
        clock-show-seconds = lib.mkDefault false;
        clock-show-weekday = lib.mkDefault true;
        color-scheme = theme.color.schemes.default.kind;

        font-antialiasing = "rgba";
        font-hinting = "slight";

        # Because it's so ingrained by both shells and even macOS defaults.
        # But it's understandable why it's not the default:
        # e.g. "C-a" is no longer for select all -- macOS can get away with this
        # because it doesn't use Ctrl as prominently as Super
        gtk-key-theme = "Emacs";
      };
    };
  }
