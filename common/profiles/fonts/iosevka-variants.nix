{ pkgs, ... }:
let
  inherit (pkgs) iosevka-bin iosevka-comfy;
  makeIosevkaVariant = variant: iosevka-bin.override { inherit variant; };
  makeIosevkaSgrVariant = variant: makeIosevkaVariant "SGr-Iosevka${variant}";
in
{
  fonts.packages = [
    iosevka-bin
    iosevka-comfy.comfy
    iosevka-comfy.comfy-motion
    iosevka-comfy.comfy-wide-duo
    iosevka-comfy.comfy-wide-motion-duo

    ##: width variants
    (makeIosevkaSgrVariant "Fixed")
    (makeIosevkaSgrVariant "Term")
  ];
}
