{
  config,
  lib,
  nixosProfiles,
  flake,
  ...
}: let
  inherit (config.networking) hostName;
  inherit (flake.inputs) nixos-apple-silicon;
  firmwareInputName = "asahi-${hostName}-firmware";
  firmwareInput = flake.perSystem.inputs'.${firmwareInputName};
in {
  imports = [
    nixosProfiles.boot.systemd-boot
    nixos-apple-silicon.nixosModules.apple-silicon-support
  ];

  hardware.asahi.peripheralFirmwareDirectory = lib.mkDefault firmwareInput.packages.default;
  hardware.asahi.addEdgeKernelConfig = lib.mkDefault true;
  boot.loader.efi.canTouchEfiVariables = lib.mkForce false;
  boot.extraModprobeConfig = ''
    options hid_apple iso_layout=0
  '';
}
