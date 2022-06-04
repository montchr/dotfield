{
  config,
  lib,
  pkgs,
  ...
}: {
  nixpkgs.config.allowUnfree = true;
  hardware.enableAllFirmware = true;
  hardware.enableRedistributableFirmware = true;

  # TODO: this may be overkill, but for the most part we need to know that
  # drivers are up to date
  boot.kernelPackages = lib.mkDefault pkgs.linuxPackages_latest;

  hardware.nvidia.modesetting.enable = true;
  services.xserver.videoDrivers = ["nvidia"];
}
