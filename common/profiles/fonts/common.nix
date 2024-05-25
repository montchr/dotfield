{
  lib,
  pkgs,
  flake,
  ...
}:
let
  inherit (flake.perSystem) packages;
in
{
  imports = [ ./iosevka-variants.nix ];

  fonts.fontDir.enable = true;
  fonts.packages = (
    with pkgs;
    [
      bakoma_ttf
      # <https://software.sil.org/charis/>
      # <https://practicaltypography.com/charter.html>
      charis-sil
      corefonts
      dejavu_fonts
      fira
      gentium
      ia-writer-duospace
      ibm-plex
      inter
      jetbrains-mono
      liberation_ttf
      (nerdfonts.override { fonts = [ "NerdFontsSymbolsOnly" ]; })
      terminus_font
    ]
  );
}
