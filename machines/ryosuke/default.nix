{ pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ./game.nix
    ./media.nix
    ./users/cdom.nix
    ./users/median.nix
  ];

  dotfield.guardian.enable = true;
  dotfield.guardian.username = "cdom";

  services.displayManager.autoLogin.enable = true;
  services.displayManager.autoLogin.user = "median";

  sops.defaultSopsFile = ./secrets/secrets.yaml;
  # Never remove old secrets (attempt to fix lockouts).
  sops.keepGenerations = 0;

  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.supportedFilesystems = [ "btrfs" ];
  boot.supportedFilesystems = [ "btrfs" ];
  boot.kernelPackages = pkgs.linuxPackages_latest;

  time.timeZone = "America/New_York";
  networking.firewall.enable = true;

  system.stateVersion = "22.05";
}
