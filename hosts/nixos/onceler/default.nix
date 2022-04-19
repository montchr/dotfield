{
  config,
  pkgs,
  lib,
  suites,
  profiles,
  hmUsers,
  ...
}: {
  imports = with suites;
    gui
    ++ personal
    ++ (with profiles; [
      users.xtallos
    ]);

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.networkmanager.enable = true;

  fileSystems."/" = {device = "/dev/disk/by-label/nixos";};

  home-manager.users.xtallos = {suites, ...}: {
    imports = [hmUsers.xtallos] ++ suites.gui;
  };
}
