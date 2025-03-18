{ pkgs, ... }:
let
  fileChooserDefaults = {
    date-format = "regular";
    location-mode = "path-bar";
    show-hidden = true;
    show-size-column = true;
    show-type-column = true;
    sort-column = "name";
    sort-directories-first = true;
    sort-order = "ascending";
    type-format = "category";
  };
in
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

    "org/gtk/gtk4/settings/file-chooser" = fileChooserDefaults // { };
    "org/gtk/settings/file-chooser" = fileChooserDefaults // { };
  };
}
