flake@{ ... }:
{
  dotfield.aspects.hardware__apple__macbook-pro-11-3.nixos = {
    imports = [ flake.config.dotfield.aspects.hardware__apple__macbook-intel.nixos ];
  };
}
