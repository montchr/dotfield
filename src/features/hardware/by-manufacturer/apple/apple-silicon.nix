{ lib, ... }:
{
  dotfield.aspects.hardware__apple__apple-silicon.nixos = {
    hardware.asahi.enable = true;

    boot.loader.systemd-boot.consoleMode = lib.mkForce "0";
    boot.loader.efi.canTouchEfiVariables = lib.mkForce false;

    # Mutually exclusive legacy Apple hardware.
    hardware.facetimehd.enable = lib.mkForce false;
    services.mbpfan.enable = lib.mkForce false;
  };
}
