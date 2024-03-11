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
      color-scheme = config.theme.color.schemes.default.kind;

      font-antialiasing = lib.mkDefault "rgba";
      font-hinting = lib.mkDefault "slight";
    };
  };
}
