{ config, lib, pkgs, suites, profiles, hmUsers, ... }:

let
  extraProfiles = with profiles; [
    users.xtallos
    virtualisation.guests.parallels
  ];
in

{
  imports = with suites;
    base
    ++ extraProfiles
    ++ [./hardware-configuration.nix];

  networking.hostName = "parrothelles"; # Define your hostname.
  time.timeZone = "America/New_York";

  users.users.xtallos.password = "xtallos";
  home-manager.users.xtallos = {suites, ...}: {
    imports = [hmUsers.xtallos] ++ suites.gui;
  };

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";
  boot.initrd.checkJournalingFS = false;

  networking.useDHCP = false;
  networking.interfaces.enp0s5.useDHCP = true;

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  services.xserver.enable = true;
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;
  services.xserver.layout = "us";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  services.pipewire = {
    enable = true;
    alsa = {
      enable = true;
      support32Bit = true;
    };
    pulse.enable = true;
  };
  hardware.pulseaudio.enable = false;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

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

  services.openssh.enable = true;

  networking.firewall.enable = false;

  system.stateVersion = "21.11";
}
