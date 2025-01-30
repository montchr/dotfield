{ pkgs, ... }:
{
  home.packages = [
    pkgs.dconf2nix # <https://github.com/gvolpe/dconf2nix>
    pkgs.dconf-editor
  ];

  # TODO: gtk4?
  qt.platformTheme.name = "gtk3";

  dconf.settings = {
    "org/gnome/desktop/interface" = {
      gtk-key-theme = "Emacs";

      font-antialiasing = "rgba";
      font-hinting = "slight";
      text-scaling-factor = 1.0;
    };
  };
}
