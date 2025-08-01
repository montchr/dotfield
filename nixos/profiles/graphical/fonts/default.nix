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
  environment.systemPackages = [ pkgs.font-manager ];
  fonts.fontDir.enable = true;
  fonts.packages = (
    let
      mkIosevkaSgrVariant =
        variant:
        pkgs.iosevka-bin.override {
          variant = "SGr-Iosevka${variant}";
        };

      baselineFonts = with pkgs; [
        corefonts
        dejavu_fonts
        ibm-plex
        iosevka
        inter
        jetbrains-mono
        nerd-fonts.symbols-only
        terminus_font
      ];

      preferredFonts = with pkgs; [
        aporetic
        (mkIosevkaSgrVariant "Term")
        flake.perSystem.packages.iosvmata-bin
        flake.perSystem.packages.pragmasevka-bin
      ];
    in
    baselineFonts
    ++ preferredFonts
    ++ (with pkgs; [
      bakoma_ttf
      # <https://software.sil.org/charis/>
      # <https://practicaltypography.com/charter.html>
      charis-sil
      # very nice website: <https://commitmono.com/>
      commit-mono
      departure-mono
      fira
      gentium
      ia-writer-duospace
      ia-writer-quattro
    ])
  );
}
