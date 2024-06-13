{ lib, pkgs, ... }:
{
  imports = [ ./iosevka-variants.nix ];

  fonts.fontconfig = {
    enable = true;
    defaultFonts = {
      monospace = lib.mkDefault [ "Iosevka" ];
      serif = lib.mkDefault [ "IBM Plex Serif" ];
      sansSerif = lib.mkDefault [ "Inter" ];
    };
  };

  fonts.fontDir.enable = true;
  fonts.packages = (
    with pkgs;
    [
      bakoma_ttf
      # <https://software.sil.org/charis/>
      # <https://practicaltypography.com/charter.html>
      charis-sil
      dejavu_fonts
      fira
      gentium
      ia-writer-duospace
      ibm-plex
      inter
      iosevka-bin
      iosevka-comfy.comfy
      iosevka-comfy.comfy-motion
      iosevka-comfy.comfy-wide-duo
      iosevka-comfy.comfy-wide-motion-duo
      jetbrains-mono
      liberation_ttf
      (nerdfonts.override { fonts = [ "NerdFontsSymbolsOnly" ]; })
      terminus_font
    ]
  );
}
