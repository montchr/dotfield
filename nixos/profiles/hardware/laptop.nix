{
  imports = [ ../networking/wifi.nix ];

  # battery info
  services.upower.enable = true;

  networking.networkmanager.wifi.powersave = true;

  home-manager.sharedModules = [ ../../../home/profiles/hardware/laptop.nix ];
}
