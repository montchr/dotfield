flake@{ ... }:
{
  dotfield.features."hardware/focusrite/scarlett-18i20-mk1".nixos = {
    imports = [ flake.config.dotfield.features.pro-audio.nixos ];

    # TODO:
    # musnix.soundcardPciId = "";
  };
}
