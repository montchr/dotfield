{
  config,
  lib,
  flake,
  ...
}:
let
  inherit (config.networking) hostName;
  inherit (flake.inputs) nixos-apple-silicon;
  firmwareInputName = "asahi-${hostName}-firmware";
  firmwareInput = flake.inputs'.${firmwareInputName};
in
{
  imports = [
    ../../boot/systemd-boot.nix

    nixos-apple-silicon.nixosModules.apple-silicon-support
  ];

  hardware.asahi.peripheralFirmwareDirectory = lib.mkDefault firmwareInput.packages.default;
  boot.loader.efi.canTouchEfiVariables = lib.mkForce false;

  hardware.facetimehd.enable = lib.mkForce false;
  services.mbpfan.enable = lib.mkForce false;
}
