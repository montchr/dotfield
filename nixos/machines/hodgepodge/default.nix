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

  # diag = w: h: sqrt(w^2 + h^2);
  # diagPx = diag 2880 1800;      => 3396.23320754
  # diagIn = 15;
  # ppi = diagPx / diagIn;        => 226.415547169
  services.xserver.dpi = 226;

  system.stateVersion = "21.11"; # Did you read the comment?
}
