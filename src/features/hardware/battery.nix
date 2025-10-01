{
  aspects.battery = {
    nixos =
      { pkgs, ... }:
      {
        services.upower.enable = true;
        networking.networkmanager.wifi.powersave = true;
      };

    home = {
      dconf.settings."org/gnome/desktop/interface".show-battery-percentage = true;
      services.poweralertd.enable = true;
    };
  };
}
