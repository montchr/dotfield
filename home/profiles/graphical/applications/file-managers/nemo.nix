{
  flake,
  config,
  lib,
  pkgs,
  ...
}:
let
  prefs = import "${flake.self}/users/${config.home.username}/preferences.nix" {
    inherit pkgs;
  };
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

  dconf.settings."org/cinnamon/desktop/applications/terminal".exec = prefs.term or "foot";
}
