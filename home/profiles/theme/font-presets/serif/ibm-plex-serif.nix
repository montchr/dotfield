{ pkgs, ... }:
{
  theme.fonts.serif = {
    name = "IBM Plex Serif";
    package = pkgs.ibm-plex;
  };
}
