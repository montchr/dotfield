{ self, ... }:
{
  dotfield.modules.defaults.nixos = {
    imports = [ self.dotfield.nixos."hardware/apple-macbook" ];

    hardware.facetimehd.enable = true;
    services.mbpfan.enable = true;
  };
}
