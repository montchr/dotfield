# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ pkgs, lib, ... }:
{
  imports = [ ./hardware-configuration.nix ];

  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "freundix"; # Define your hostname.

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.cdom = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    packages = [ ];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget
    curl
    git
    tealdeer
    fd
    ripgrep
    neovim
    bat
    rsync
  ];

  services.openssh.enable = true;
  services.openssh.settings.PasswordAuthentication = lib.mkForce true;
  services.openssh.settings.PermitRootLogin = "yes";

  users.users.root.initialPassword = "root";

  networking.firewall.enable = false;

  system.stateVersion = "22.11"; # Did you read the comment?
}
