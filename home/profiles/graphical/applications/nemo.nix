{ lib, pkgs, ... }:
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
      "inode/directory" = [ "nemo.desktop" ];
      "application/x-gnome-saved-search" = [ "nemo.desktop" ];
    };
  };

  dconf.settings."org/cinnamon/desktop/applications/terminal" = {
    exec = lib.mkDefault "foot";
  };
}
