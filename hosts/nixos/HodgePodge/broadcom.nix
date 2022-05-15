{
  config,
  lib,
  pkgs,
  ...
}: {
  # Required for broadcom driver support.
  nixpkgs.config.allowUnfree = true;
  hardware.enableRedistributableFirmware = true;
  hardware.enableAllFirmware = true;

  environment.systemPackages = with pkgs; [
    linuxPackages.broadcom_sta
  ];
}
