{ lib, ... }:
{
  imports = [
    # Enabled with `dotfield.hardware.keyboard.remapping`
    ./__keyd
    ./__kmonad
    ./__kanata

    ./zsa.nix
  ];

  dotfield.hardware.keyboard.remapping = {
    enable = lib.mkDefault true;
    provider = lib.mkDefault "keyd";
  };
}
