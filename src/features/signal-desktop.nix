{ self, lib, ... }:
{
  dotfield.features.graphical.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.signal-desktop
      ];

      dconf.settings."org/gnome/desktop/notifications/application/signal-desktop" =
        lib.mkIf self.dotfield.graphical.nixos.services.desktopManager.gnome.enable
          {
            application-id = "signal-desktop.desktop";
          };
    };
}
