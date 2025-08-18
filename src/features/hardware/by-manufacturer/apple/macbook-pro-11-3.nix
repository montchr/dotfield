flake@{ ... }:
{
  dotfield.features.hardware__apple__macbook-pro-11-3.nixos = {
    imports = [ flake.config.dotfield.features.hardware__apple__macbook-intel.nixos ];
  };
}
