{
  flake,
  lib,
  pkgs,
  ...
}:
{
  fonts.fontconfig = {
    enable = true;
  };

  fonts.fontDir.enable = true;
  fonts.packages = (
    with pkgs;
    [
      flake.perSystem.legacyPackages.aporetic.sans
      flake.perSystem.legacyPackages.aporetic.sans-mono
      flake.perSystem.legacyPackages.aporetic.serif
      flake.perSystem.legacyPackages.aporetic.serif-mono

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
      # iosevka-bin
      # iosevka-comfy.comfy
      # iosevka-comfy.comfy-motion
      # iosevka-comfy.comfy-wide-duo
      # iosevka-comfy.comfy-wide-motion-duo
      jetbrains-mono
      liberation_ttf
      nerd-fonts.symbols-only
      terminus_font
    ]
  );
}
