{ self, ... }:
{
  dotfield.modules."hardware/focusrite/scarlett-18i20-mk1".nixos = {
    imports = [ self.dotfield.nixos.pro-audio ];

    # TODO:
    # musnix.soundcardPciId = "";
  };
}
