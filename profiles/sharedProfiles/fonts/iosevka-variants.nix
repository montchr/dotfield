{pkgs, ...}: let
  inherit (pkgs) iosevka-bin iosevka-comfy;
  makeIosevkaVariant = variant: iosevka-bin.override {inherit variant;};
  makeIosevkaSgrVariant = variant: makeIosevkaVariant "sgr-iosevka-${variant}";
in {
  fonts.fonts = [
    iosevka-bin
    iosevka-comfy.comfy
    iosevka-comfy.comfy-motion
    iosevka-comfy.comfy-wide-duo
    iosevka-comfy.comfy-wide-motion-duo

    ##: stylistic variants
    (makeIosevkaVariant "aile")
    (makeIosevkaVariant "etoile")
    # consolas style
    (makeIosevkaVariant "ss03")
    # pragmatapro style
    (makeIosevkaVariant "ss08")
    # jetbrains mono style
    (makeIosevkaVariant "ss14")

    ##: width variants
    (makeIosevkaSgrVariant "fixed")
    (makeIosevkaSgrVariant "term")
  ];
}
