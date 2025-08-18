flake@{ ... }:
{
  dotfield.features.hardware__apple__macbook-intel.nixos = {
    imports = [ flake.config.dotfield.features.hardware__apple__macbook.nixos ];

    hardware.facetimehd.enable = true;
    services.mbpfan.enable = true;
  };
}
