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
  users.cdom.aspects.graphical.home = {
    dconf.settings = {
      "org/gtk/gtk4/settings/file-chooser" = fileChooserDefaults // { };
      "org/gtk/settings/file-chooser" = fileChooserDefaults // { };
    };
  };
}
