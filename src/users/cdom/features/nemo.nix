flake@{ ... }:
{
  dotfield.users.cdom.features.wayland.home =
    { config, pkgs, ... }:
    let
      prefs = flake.config.dotfield.meta.users.${config.home.username}.preferences;
      nemoPackage = pkgs.nemo-with-extensions;
    in
    {
      home.packages = [ nemoPackage ];

      xdg.desktopEntries.nemo = {
        name = "Nemo";
        exec = "${nemoPackage}/bin/nemo";
      };

      xdg.mimeApps = {
        enable = true;
        defaultApplications = {
          "application/x-gnome-saved-search" = [ "nemo.desktop" ];
        };
      };

      dconf.settings."org/cinnamon/desktop/applications/terminal".exec = prefs.term;
      dconf.settings."org/nemo/desktop" = {
        show-desktop-icons = false;
      };

    };
}
