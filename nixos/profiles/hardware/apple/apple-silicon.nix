{ lib, flake, ... }:
{
  imports = [
    ../../boot/systemd-boot.nix
  ];

  boot.loader.systemd-boot.consoleMode = lib.mkForce "0";
  boot.loader.efi.canTouchEfiVariables = lib.mkForce false;

  # Mutually exclusive legacy Apple hardware.
  hardware.facetimehd.enable = lib.mkForce false;
  services.mbpfan.enable = lib.mkForce false;

  hardware.asahi.enable = true;
}
