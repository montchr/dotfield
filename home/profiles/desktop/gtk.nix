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
      color-scheme = lib.mkDefault "prefer-dark";
      font-antialiasing = lib.mkDefault "rgba";
      font-hinting = lib.mkDefault "slight";
    };
  };
}
