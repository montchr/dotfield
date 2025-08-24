flake@{ ... }:
{
  dotfield.aspects.hardware__apple__macbook-intel.nixos = {
    imports = [ flake.config.dotfield.aspects.hardware__apple__macbook.nixos ];

    hardware.facetimehd.enable = true;
    services.mbpfan.enable = true;
  };
}
