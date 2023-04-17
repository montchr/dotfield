{pkgs, ...}: let
  inherit (pkgs) iosevka-bin;
  makeIosevkaVariant = variant: iosevka-bin.override {inherit variant;};
  makeIosevkaSgrVariant = variant: makeIosevkaVariant "sgr-iosevka-${variant}";
in {
  fonts.fonts = [
    iosevka-bin

    ##: stylistic variants
    (makeIosevkaVariant "aile")
    (makeIosevkaVariant "etoile")
    # pragmatapro style
    (makeIosevkaVariant "ss08")

    ##: width variants
    (makeIosevkaSgrVariant "fixed")
    (makeIosevkaSgrVariant "term")
  ];
}
