{ pkgs, ... }:
let
  inherit (pkgs) iosevka-bin iosevka-comfy;
  makeIosevkaVariant = variant: iosevka-bin.override { inherit variant; };
  makeIosevkaSgrVariant = variant: makeIosevkaVariant "SGr-Iosevka${variant}";
in
{
  fonts.packages = [
    ##: width variants
    (makeIosevkaSgrVariant "Fixed")
    (makeIosevkaSgrVariant "Term")
  ];
}
