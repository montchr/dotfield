{
  aspects.battery = {
    nixos =
      { pkgs, ... }:
      {
        services.upower.enable = true;
        networking.networkmanager.wifi.powersave = true;
        environment.systemPackages = [ pkgs.poweralertd ];
      };

    home = {
      dconf.settings."org/gnome/desktop/interface".show-battery-percentage = true;
    };
  };
}
