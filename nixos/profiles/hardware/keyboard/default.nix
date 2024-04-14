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

  hardware.keyboard.keyboardio.enable = true;

  hardware.keyboard.zsa.enable = true;
  environment.systemPackages = [
    pkgs.wally-cli
  ];
}
