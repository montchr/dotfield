{ lib, ... }:
{
  imports = [
    ../laptop.nix
    ./macbook.nix
  ];

  hardware.facetimehd.enable = lib.mkDefault true;
  services.mbpfan.enable = true;
}
