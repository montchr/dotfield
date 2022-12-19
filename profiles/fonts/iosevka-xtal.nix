{
  inputs',
  self,
  ...
}: let
  inherit
    (inputs'.iosevka-xtal.packages)
    iosevka-xtal
    iosevka-xtal-term
    ;
in {
  imports = [(self + "/profiles/core/substituters/iosevka-xtal.nix")];
  fonts.fonts = [
    iosevka-xtal
    iosevka-xtal-term
  ];
}
