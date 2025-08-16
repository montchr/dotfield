flake@{ ... }:
{
  dotfield.features.hardware__apple__macbook-intel.nixos = {
    imports = [ flake.config.dotfield.features.hardware__apple-macbook ];

    hardware.facetimehd.enable = true;
    services.mbpfan.enable = true;
  };
}
