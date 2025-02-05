{ flake, ... }:
{
  theme.fonts.serif = {
    name = "Aporetic Serif";
    package = flake.perSystem.legacyPackages.aporetic.serif;
    psNamespace = "Aporetic-Serif";
  };
}
