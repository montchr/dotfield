flake@{ ... }:
{
  dotfield.features.hardware__focusrite__scarlett-18i20-mk1.nixos = {
    imports = [ flake.config.dotfield.features.pro-audio.nixos ];

    # TODO:
    # musnix.soundcardPciId = "";
  };
}
