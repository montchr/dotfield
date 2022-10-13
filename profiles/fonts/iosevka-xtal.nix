{
  system,
  self,
  ...
}: let
  inherit
    (self.inputs.iosevka-xtal.packages.${system})
    iosevka-xtal
    iosevka-xtal-term
    ;
in {
  fonts.fonts = [
    iosevka-xtal
    iosevka-xtal-term
  ];
}
