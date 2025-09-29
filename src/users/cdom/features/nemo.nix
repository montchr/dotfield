flake@{ ... }:
{
  users.cdom.aspects.noop.home =
    {
      config,
      pkgs,
      ...
    }:
    let
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

      dconf.settings."org/nemo/desktop" = {
        show-desktop-icons = false;
      };
    };
}
