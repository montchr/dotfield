{ pkgs, ... }:
{
  theme.font.sansSerif = {
    name = "Inter";
    package = pkgs.inter;
  };
}
