{flake, ...}: let
  inherit (flake.perSystem.packages) berkeley-mono;
in {
  fonts.fonts = [berkeley-mono];

  # theme.fonts.mono = {
  #   name = "Berkeley Mono";
  #   # package = berkeley-mono;
  # };
}
