{
  config,
  lib,
  pkgs,
  suites,
  profiles,
  hmUsers,
  primaryUser,
  ...
}: let
  secretsDir = ../../../secrets;
in {
  imports =
    (with suites; workstation)
    ++ (with profiles; [
      virtualisation.guests.parallels
    ])
    ++ [./hardware-configuration.nix];

  users.users.seadoom = {
    extraGroups = ["wheel"];
    password = "seadoom";
    isNormalUser = true;
    openssh.authorizedKeys.keys = primaryUser.authorizedKeys;
  };
  home-manager.users.seadoom = hmArgs: {
    imports =
      [hmUsers.seadoom]
      ++ (with hmArgs.suites; desktop);
  };

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";
  boot.initrd.checkJournalingFS = false;

  networking.useDHCP = false;
  networking.interfaces.enp0s5.useDHCP = true;
  networking.firewall.enable = false;

  security.sudo.wheelNeedsPassword = false;

  environment.variables.DOTFIELD_DIR = "/etc/nixos";

  users.mutableUsers = false;

  system.stateVersion = "21.11";
}
