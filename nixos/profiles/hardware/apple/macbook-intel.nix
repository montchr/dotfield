{ lib, ... }:
{
  imports = [
    ../laptop.nix
    ./macbook.nix
  ];

  hardware.facetimehd.enable = true;
  services.mbpfan.enable = true;
}
