{ pkgs, lib, ... }:
{
  imports = [
    # Enabled with `dotfield.hardware.keyboard.remapping`
    ./__keyd
    ./__kmonad
    ./__kanata
  ];

  dotfield.hardware.keyboard.remapping = {
    enable = lib.mkDefault true;
    provider = lib.mkDefault "keyd";
  };

  # Required to support flashing firmware.
  dotfield.guardian.extraGroups = [ "plugdev" ];

  hardware.keyboard.qmk.enable = true;

  hardware.keyboard.keyboardio.enable = true;

  hardware.keyboard.zsa.enable = true;
  environment.systemPackages = [
    pkgs.keymapp
    pkgs.wally-cli
  ];
}
