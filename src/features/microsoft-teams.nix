flake@{ lib, ... }:
{
  dotfield.features.workstation.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.teams-for-linux
      ];

      dconf.settings."org/gnome/desktop/notifications/application/teams".application-id =
        lib.mkIf flake.config.dotfield.graphical.nixos.services.desktopManager.gnome.enable "teams.desktop";
    };
}
