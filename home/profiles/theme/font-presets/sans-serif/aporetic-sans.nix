{ pkgs, ... }:
{
  theme.font.sansSerif = {
    name = "Aporetic Sans";
    package = pkgs.aporetic;
    psNamespace = "Aporetic-Sans";
  };
}
