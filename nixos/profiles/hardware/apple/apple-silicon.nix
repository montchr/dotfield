{
  config,
  lib,
  profiles,
  flake,
  ...
}:
let
  inherit (config.networking) hostName;
  inherit (flake.inputs) nixos-apple-silicon;
  firmwareInputName = "asahi-${hostName}-firmware";
  firmwareInput = flake.perSystem.inputs'.${firmwareInputName};
in
{
  imports = [
    profiles.boot.systemd-boot
    nixos-apple-silicon.nixosModules.apple-silicon-support
  ];

  hardware.asahi.peripheralFirmwareDirectory = lib.mkDefault firmwareInput.packages.default;
  boot.loader.efi.canTouchEfiVariables = lib.mkForce false;

  hardware.facetimehd.enable = lib.mkForce false;
  services.mbpfan.enable = lib.mkForce false;
}
