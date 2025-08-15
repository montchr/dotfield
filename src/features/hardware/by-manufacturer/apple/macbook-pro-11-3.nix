flake@{ ... }:
{
  dotfield.features."hardware/apple/macbook-pro-11-3".nixos = {
    imports = [ flake.config.dotfield.nixos."hardware/apple-macbook-intel" ];
  };
}
