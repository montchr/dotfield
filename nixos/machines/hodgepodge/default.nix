{
  config,
  lib,
  pkgs,
  primaryUser,
  profiles,
  peers,
  ...
}: let
  inherit (config.networking) hostName;

  # Shared by both "predictable" interface names:
  # - enp0s20u1
  # - wlp3s0
  interface = "eth0";
in {
  imports = [
    ./hardware-configuration.nix
    ./profiles/sops.nix
    ./users
  ];

  boot.loader.efi.canTouchEfiVariables = true;
  services.printing.enable = true;
  hardware.facetimehd.enable = true;

  networking.usePredictableInterfaceNames = false;
  networking.firewall.enable = true;

  system.stateVersion = "21.11"; # Did you read the comment?
}
