{
  dotfield.aspects.hardware__battery.nixos =
    { pkgs, ... }:
    {
      services.upower.enable = true;
      networking.networkmanager.wifi.powersave = true;
      environment.systemPackages = [ pkgs.poweralertd ];
    };
}
