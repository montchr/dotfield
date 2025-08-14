{ self, ... }:
{
  dotfield.features."hardware/apple/macbook-pro-11-3".nixos = {
    imports = [ self.dotfield.nixos."hardware/apple-macbook-intel" ];
  };
}
