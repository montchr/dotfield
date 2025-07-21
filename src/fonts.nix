{
  flake.modules.nixos.graphical =
    { pkgs, ... }:
    {
      fonts.fontconfig = {
        enable = true;
      };
      environment.systemPackages = [ pkgs.font-manager ];
      fonts.fontDir.enable = true;
      fonts.packages = (
        with pkgs;
        [
          aporetic
          bakoma_ttf
          # <https://software.sil.org/charis/>
          # <https://practicaltypography.com/charter.html>
          charis-sil
          corefonts
          dejavu_fonts
          departure-mono
          fira
          gentium
          ia-writer-duospace
          ia-writer-quattro
          ibm-plex
          inter
          jetbrains-mono
          nerd-fonts.symbols-only
          terminus_font
        ]
      );
    };

}
