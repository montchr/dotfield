{
  # battery info
  services.upower.enable = true;

  home-manager.sharedModules = [ ../../../home/profiles/hardware/laptop.nix ];
}
