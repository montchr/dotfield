{
  iosevka,
  lib,
  fontWeights,
}: let
  mkFontFamily = import ./mkFontFamily.nix {inherit iosevka lib fontWeights;};
  mkVariant = args: let
    defaults = {
      slopes = ["italic"];
      weights = ["light" "regular" "semibold" "bold"];
    };
  in
    mkFontFamily (defaults // args);
in {
  default = mkFontFamily {};
  term = mkVariant {spacing = "term";};
  fixed = mkVariant {spacing = "fixed";};
}
