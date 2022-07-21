{
  config,
  lib,
  pkgs,
  ...
}: {
  nixpkgs.config.allowUnfree = true;
  hardware.enableAllFirmware = true;
  hardware.enableRedistributableFirmware = true;
  boot.kernelPackages = lib.mkDefault pkgs.linuxPackages_latest;
  services.xserver.videoDrivers = ["nvidia"];
}
