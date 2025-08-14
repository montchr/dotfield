{ lib, self, ... }:
{
  dotfield.features.workstation.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.teams-for-linux
      ];

      dconf.settings."org/gnome/desktop/notifications/application/teams".application-id =
        lib.mkIf self.dotfield.graphical.nixos.services.desktopManager.gnome.enable "teams.desktop";
    };
}
