{
  config,
  lib,
  pkgs,
  primaryUser,
  profiles,
  suites,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
    ./profiles/sops.nix
    ./users
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.useDHCP = false;
  networking.interfaces.enp0s20u1.useDHCP = true;

  services.printing.enable = true;
  hardware.facetimehd.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    _1password-gui
    _1password
  ];

  networking.firewall.enable = false;

  system.stateVersion = "21.11"; # Did you read the comment?

  nixpkgs.config.allowUnfree = true;
}
