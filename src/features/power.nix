{
  # FIXME: should be workstation
  aspects.graphical.nixos = {
    services.logind.extraConfig = ''
      HandlePowerKey=suspend
    '';

    services.power-profiles-daemon.enable = true;
  };

  aspects.graphical.home = {
    dconf.settings."org/gnome/settings-daemon/plugins/power" = {
      power-button-action = "suspend";
      # FIXME: this isn't great, should suspend after a while
      sleep-inactive-ac-type = "nothing";
    };
  };
}
