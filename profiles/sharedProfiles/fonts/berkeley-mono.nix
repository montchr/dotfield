{flake, ...}: let
  inherit (flake.perSystem.packages) berkeley-mono;
in {
  fonts.fonts = [berkeley-mono];
}
