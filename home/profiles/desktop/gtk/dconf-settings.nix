{ lib, config, ... }:

{
  dconf.settings = {
    "org/gnome/desktop/interface" = {
      color-scheme = config.theme.color.schemes.default.kind;

      font-antialiasing = "rgba";
      font-hinting = "slight";
    };
  };
}
