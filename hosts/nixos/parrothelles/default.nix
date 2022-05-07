{
  config,
  lib,
  pkgs,
  suites,
  profiles,
  hmUsers,
  ...
}:

let
  secretsDir = ../../../secrets;
in

{
  imports = with suites;
    graphical
    ++ personal
    ++ (with profiles; [
      audio
      users.xtallos
      virtualisation.guests.parallels
    ])
    ++ [./hardware-configuration.nix];

  users.users.xtallos = {
    password = "xtallos";
    openssh.authorizedKeys.keys = (import "${secretsDir}/authorized-keys.nix");
  };

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

  services.openssh.enable = true;
  # TODO: should this be locked down further?
  services.openssh.openFirewall = true;

  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = false;

  security.doas.enable = true;
  security.doas.wheelNeedsPassword = false;

  environment.variables.DOTFIELD_DIR = "/etc/nixos";

  users.mutableUsers = false;

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
