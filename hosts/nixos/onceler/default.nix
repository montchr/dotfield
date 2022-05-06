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
    graphical
    ++ personal
    ++ tangible
    ++ (with profiles; [
      users.xtallos
    ]);

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.networkmanager.enable = true;

  fileSystems."/" = {device = "/dev/disk/by-label/nixos";};

  home-manager.users.xtallos = {profiles, suites, ...}: {
    imports =
      [hmUsers.xtallos]
      ++ (with suites; graphical)
      ++ (with profiles; [mail]);
  };
}
