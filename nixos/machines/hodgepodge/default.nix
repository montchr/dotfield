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

  boot.loader.efi.canTouchEfiVariables = true;

  networking.useDHCP = false;
  networking.interfaces.enp0s20u1.useDHCP = true;
  networking.firewall.enable = true;

  services.printing.enable = true;
  hardware.facetimehd.enable = true;

  system.stateVersion = "21.11"; # Did you read the comment?
}
