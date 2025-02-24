{ pkgs, ... }:
{
  theme.fonts.sansSerif = {
    name = "Aporetic Sans";
    package = pkgs.aporetic;
    psNamespace = "Aporetic-Sans";
  };
}
