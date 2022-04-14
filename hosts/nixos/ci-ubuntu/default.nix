{
  config,
  lib,
  pkgs,
  suites,
  ...
}: {
  imports = with suites; minimal;

  my.username = "runner";

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.networkmanager.enable = true;

  fileSystems."/" = {device = "/dev/disk/by-label/nixos";};
}
