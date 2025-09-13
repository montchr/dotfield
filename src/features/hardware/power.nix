{
  aspects.workstation.nixos = {
    services.logind.settings.Login = {
      HandlePowerKey = "suspend";
    };

    services.power-profiles-daemon.enable = true;
  };

  aspects.workstation.home = {
    dconf.settings."org/gnome/settings-daemon/plugins/power" = {
      power-button-action = "suspend";
      # FIXME: this isn't great, should suspend after a while
      sleep-inactive-ac-type = "nothing";
    };
  };
}
