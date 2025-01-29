{
  pkgs,
  lib,
  config,
  ...
}:
{
  home.packages = [
    pkgs.dconf2nix # <https://github.com/gvolpe/dconf2nix>
    pkgs.dconf-editor
  ];

  dconf.settings = {
    "org/gnome/desktop/interface" = {
      font-antialiasing = "rgba";
      font-hinting = "slight";
    };
  };
}
