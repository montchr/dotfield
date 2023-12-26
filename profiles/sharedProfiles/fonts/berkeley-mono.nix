{flake, ...}: let
  inherit (flake.perSystem.packages) berkeley-mono;
in {
  fonts.packages = [berkeley-mono];
}
