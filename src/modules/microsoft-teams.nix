{ lib, self, ... }:
{
  dotfield.modules.graphical.home =
    { pkgs, ... }:
    {

      dconf.settings."org/gnome/desktop/notifications/application/teams".application-id =
        lib.mkIf self.dotfield.graphical.nixos.services.desktopManager.gnome.enable "teams.desktop";

    };
}
