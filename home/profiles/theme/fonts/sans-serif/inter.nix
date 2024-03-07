{ lib, pkgs, ... }:
{
  theme.fonts.sansSerif = lib.mkDefault {
    name = "Inter";
    package = pkgs.inter;
  };
}
