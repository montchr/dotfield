{ lib, pkgs, ... }:
{
  theme.fonts.serif = lib.mkDefault {
    name = "IBM Plex Serif";
    package = pkgs.ibm-plex;
  };
}
