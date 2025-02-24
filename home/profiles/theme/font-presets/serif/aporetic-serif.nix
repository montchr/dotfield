{ pkgs, ... }:
{
  theme.fonts.serif = {
    name = "Aporetic Serif";
    package = pkgs.aporetic;
    psNamespace = "Aporetic-Serif";
  };
}
