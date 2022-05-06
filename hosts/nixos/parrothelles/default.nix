{
  config,
  lib,
  pkgs,
  suites,
  profiles,
  hmUsers,
  ...
}: {
  imports = with suites;
    graphical
    ++ personal
    ++ (with profiles; [
      audio
      users.xtallos
      virtualisation.guests.parallels
    ])
    ++ [./hardware-configuration.nix];

  users.users.xtallos.password = "xtallos";
  home-manager.users.xtallos = {profiles, suites, ...}: {
    imports =
      [hmUsers.xtallos]
      ++ (with suites; graphical)
      ++ (with profiles; [mail]);
  };

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";
  boot.initrd.checkJournalingFS = false;

  networking.useDHCP = false;
  networking.interfaces.enp0s5.useDHCP = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    _1password-gui
    coreutils
    fd
    firefox
    git
    httpie
    kitty
    ripgrep
    vim
    vscodium
    wget
  ];

  networking.firewall.enable = false;

  system.stateVersion = "21.11";
}
