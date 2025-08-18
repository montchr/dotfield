flake@{ lib, ... }:
{
  dotfield.features.workstation.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.teams-for-linux
      ];

      dconf.settings."org/gnome/desktop/notifications/application/teams".application-id =
        lib.mkIf
          (flake.config.dotfield.features.graphical.nixos.services.desktopManager.gnome.enable or false)
          "teams.desktop";
    };
}
