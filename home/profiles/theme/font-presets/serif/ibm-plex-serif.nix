{ pkgs, ... }:
{
  theme.font.serif = {
    name = "IBM Plex Serif";
    package = pkgs.ibm-plex;
  };
}
