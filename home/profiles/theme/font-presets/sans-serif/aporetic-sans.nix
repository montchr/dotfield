{ flake, ... }:
{
  theme.fonts.sansSerif = {
    name = "Aporetic Sans";
    package = flake.perSystem.legacyPackages.aporetic.sans;
    psNamespace = "Aporetic-Sans";
  };
}
