{
  config,
  lib,
  flake,
  ...
}:
let
  inherit (config.networking) hostName;
  inherit (flake.inputs) nixos-apple-silicon;
  firmwareInputName =
    let
      hostName' =
        builtins.replaceStrings
          [
            "-installer"
            "-iso"
          ]
          [
            ""
            ""
          ]
          hostName;
    in
    "asahi-${hostName'}-firmware";
  firmwareInput = flake.perSystem.inputs'.${firmwareInputName};
in
{
  imports = [
    nixos-apple-silicon.nixosModules.apple-silicon-support

    ../../boot/systemd-boot.nix
  ];

  hardware.asahi.peripheralFirmwareDirectory = firmwareInput.packages.default;

  boot.loader.systemd-boot.consoleMode = lib.mkForce "0";
  boot.loader.efi.canTouchEfiVariables = lib.mkForce false;

  # Mutually exclusive legacy Apple hardware.
  hardware.facetimehd.enable = lib.mkForce false;
  services.mbpfan.enable = lib.mkForce false;
}
