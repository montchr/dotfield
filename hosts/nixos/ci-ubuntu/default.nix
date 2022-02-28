{ config, lib, pkgs, suites, ... }:

{
  imports = with suites; (
    linux-minimal
    ++ developer
  );

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.networkmanager.enable = true;

  fileSystems."/" = { device = "/dev/disk/by-label/nixos"; };
}
