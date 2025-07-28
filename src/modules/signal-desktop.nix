{ self, lib, ... }:
{
  dotfield.modules.graphical.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.signal-desktop
      ];

      dconf.settings."org/gnome/desktop/notifications/application/firefox" =
        lib.mkIf self.dotfield.graphical.nixos.services.desktopManager.gnome.enable
          {
            application-id = "firefox.desktop";
          };
    };
}
