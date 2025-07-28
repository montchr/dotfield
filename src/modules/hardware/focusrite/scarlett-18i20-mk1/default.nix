{ self, ... }:
{
  dotfield.modules.workstation.nixos = {
    imports = [ self.dotfield.nixos.pro-audio ];

    # TODO:
    # musnix.soundcardPciId = "";
  };
}
