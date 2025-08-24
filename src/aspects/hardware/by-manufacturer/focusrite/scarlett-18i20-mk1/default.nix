flake@{ ... }:
{
  dotfield.aspects.hardware__focusrite__scarlett-18i20-mk1.nixos = {
    imports = [ flake.config.dotfield.aspects.pro-audio.nixos ];

    # TODO:
    # musnix.soundcardPciId = "";
  };
}
