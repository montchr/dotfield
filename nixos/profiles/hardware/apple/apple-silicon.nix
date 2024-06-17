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
  firmwareInput = flake.perSystem.inputs'.${firmwareInputName};
in
{
  imports = [
    nixos-apple-silicon.nixosModules.apple-silicon-support

    ../../boot/systemd-boot.nix
  ];

  hardware.asahi.peripheralFirmwareDirectory = lib.mkDefault firmwareInput.packages.default;
  boot.loader.efi.canTouchEfiVariables = lib.mkForce false;

  hardware.facetimehd.enable = lib.mkForce false;
  services.mbpfan.enable = lib.mkForce false;
}
