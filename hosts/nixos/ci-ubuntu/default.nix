{
  config,
  lib,
  pkgs,
  profiles,
  ...
}: {
  imports = [
    profiles.users.nixos
    profiles.users.root
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # networking.networkmanager.enable = true;

  fileSystems."/" = {device = "/dev/disk/by-label/nixos";};
}
