{ moduleWithSystem, ... }:
{
  aspects.graphical.nixos = moduleWithSystem (
    perSystem@{ config, ... }:
    { pkgs, ... }:
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

          preferredFonts = [
            pkgs.aporetic
            (mkIosevkaSgrVariant "Term")
            perSystem.config.packages.astrata
            perSystem.config.packages.iosvmata-bin
            perSystem.config.packages.pragmasevka-bin
          ];
        in
        baselineFonts
        ++ preferredFonts
        ++ (with pkgs; [
          atkinson-hyperlegible-mono

          atkinson-hyperlegible-next
          bakoma_ttf
          # <https://software.sil.org/charis/>
          # <https://practicaltypography.com/charter.html>
          charis-sil
          commit-mono
          departure-mono
          fira
          libre-baskerville
          gentium
        ])
      );
    }
  );

  aspects.graphical.home = {
    fonts.fontconfig.enable = true;
  };
}
