{ pkgs, ... }:
{
  theme.fonts.sansSerif = {
    name = "Inter";
    package = pkgs.inter;
  };
}
