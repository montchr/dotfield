{
  dotfield.features.workstation.nixos = {
    services.Logind.extraConfig = ''
      HandlePowerKey=suspend
    '';
    services.power-profiles-daemon.enable = true;
  };

  dotfield.features.workstation.home = {
    dconf.settings."org/gnome/settings-daemon/plugins/power" = {
      power-button-action = "suspend";
      # FIXME: this isn't great, should suspend after a while
      sleep-inactive-ac-type = "nothing";
    };
  };
}
