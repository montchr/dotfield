{ lib, ... }:
{

  services.logind.extraConfig = ''
    HandlePowerKey=suspend
  '';

  services.power-profiles-daemon.enable = true;

  home-manager.sharedModules = [
    {
      dconf.settings."org/gnome/settings-daemon/plugins/power" = {
        power-button-action = "suspend";
        # FIXME: this isn't great, should suspend after a while
        sleep-inactive-ac-type = "nothing";
      };
    }
  ];
}
