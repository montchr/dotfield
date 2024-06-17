{
  pkgs,
  lib,
  config,
  ...
}:
{
  home.packages = [
    pkgs.dconf2nix # <https://github.com/gvolpe/dconf2nix>
  ];

  dconf.settings = {
    "org/gnome/desktop/interface" = {
      color-scheme = "prefer-dark";
      font-antialiasing = "rgba";
      font-hinting = "slight";
    };
  };
}
