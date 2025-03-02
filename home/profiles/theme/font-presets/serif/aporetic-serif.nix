{ pkgs, ... }:
{
  theme.font.serif = {
    name = "Aporetic Serif";
    package = pkgs.aporetic;
    psNamespace = "Aporetic-Serif";
  };
}
