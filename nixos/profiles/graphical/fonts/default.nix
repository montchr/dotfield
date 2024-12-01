{ lib, pkgs, ... }:
{
  imports = [ ./iosevka-variants.nix ];

  fonts.fontconfig = {
    enable = true;
    defaultFonts = {
      monospace = [ "Iosevka" ];
      serif = [ "IBM Plex Serif" ];
      sansSerif = [ "Inter" ];
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
      corefonts
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
      nerd-fonts.symbols-only
      terminus_font
    ]
  );
}
