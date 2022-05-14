{
  config,
  lib,
  pkgs,
  ...
}: {
  services.xserver.enable = lib.mkForce true;
  services.xserver.desktopManager.plasma5.enable = lib.mkForce true;
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.displayManager.lightdm.enable = false;

  environment.systemPackages = with pkgs; [
    libnotify
    plasma-systemmonitor
  ];
}
