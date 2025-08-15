flake@{ ... }:
{
  dotfield.features."hardware/apple/macbook-intel".nixos = {
    imports = [ flake.config.dotfield.nixos."hardware/apple-macbook" ];

    hardware.facetimehd.enable = true;
    services.mbpfan.enable = true;
  };
}
