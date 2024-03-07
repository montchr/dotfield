# TODO: rename since this is also gtk stuff
{ lib, config, ... }:
let
  inherit (config) theme;
in
with lib.hm.gvariant;
{
  dconf.settings = {
    "org/gnome/desktop/interface" = {
      color-scheme = theme.color.schemes.default.kind;

      font-antialiasing = "rgba";
      font-hinting = "slight";
      # TODO:
      # text-scaling-factor = 1.0;
    };
  };
}
