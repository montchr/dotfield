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
  iosevka-xtal = mkFontFamily {};
  iosevka-xtal-term = mkVariant {spacing = "term";};
  iosevka-xtal-fixed = mkVariant {spacing = "fixed";};
}
