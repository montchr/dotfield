{
  config,
  lib,
  pkgs,
  suites,
  profiles,
  hmUsers,
  ...
}: let
  secretsDir = ../../../secrets;
in {
  imports = with suites;
    graphical
    ++ personal
    ++ (with profiles; [
      audio
      users.seadoom
      virtualisation.guests.parallels
    ])
    ++ [./hardware-configuration.nix];

  users.users.seadoom = {
    password = "seadoom";
    openssh.authorizedKeys.keys = import "${secretsDir}/authorized-keys.nix";
  };

  home-manager.users.xtallos = {
    profiles,
    suites,
    ...
  }: {
    imports =
      [hmUsers.seadoom]
      ++ (with suites; graphical)
      ++ (with profiles; []);
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
