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
  imports = [(self + "/profiles/core/substituters/iosevka-xtal.nix")];
  fonts.fonts = [
    iosevka-xtal
    iosevka-xtal-term
  ];
}
