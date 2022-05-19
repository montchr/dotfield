{
  config,
  lib,
  pkgs,
  profiles,
  ...
}: {
  imports = with profiles; [users.nixos users.root];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # networking.networkmanager.enable = true;

  fileSystems."/" = {device = "/dev/disk/by-label/nixos";};
}
