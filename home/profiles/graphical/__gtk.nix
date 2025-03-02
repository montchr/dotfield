{ pkgs, ... }:
{
  home.packages = [
    pkgs.dconf2nix # <https://github.com/gvolpe/dconf2nix>
    pkgs.dconf-editor
  ];

  dconf.settings = {
    "org/gnome/desktop/interface" = {
      gtk-key-theme = "Emacs";

      font-antialiasing = "rgba";
      font-hinting = "slight";
      text-scaling-factor = 1.0;
    };
  };
}
