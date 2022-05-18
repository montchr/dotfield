{
  config,
  pkgs,
  lib,
  suites,
  profiles,
  hmUsers,
  ...
}: {
  imports =
    suites.personal
    ++ suites.tangible
    ++ suites.workstation
    ++ [
      profiles.users.xtallos
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.networkmanager.enable = true;

  fileSystems."/" = {device = "/dev/disk/by-label/nixos";};

  home-manager.users.xtallos = hmArgs @ {...}: {
    imports =
      [hmUsers.xtallos]
      ++ (with hmArgs.suites; graphical)
      ++ (with hmArgs.profiles; [mail]);
  };
}
