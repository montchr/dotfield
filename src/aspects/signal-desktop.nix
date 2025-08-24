flake@{ lib, ... }:
{
  dotfield.aspects.graphical.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.signal-desktop
      ];

      dconf.settings."org/gnome/desktop/notifications/application/signal-desktop" = {
        application-id = "signal-desktop.desktop";
      };
    };
}
