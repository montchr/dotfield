{ moduleWithSystem, ... }:
{
  aspects.graphical.nixos = moduleWithSystem (
    perSystem@{ config, ... }:
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.font-manager ];

      nixpkgs.config.input-fonts.acceptLicense = true;

      fonts.fontconfig.enable = true;
      fonts.fontDir.enable = true;
      fonts.packages = (
        let
          baselineFonts = with pkgs; [
            corefonts
            dejavu_fonts
            ibm-plex
            input-fonts
            iosevka
            inter
            jetbrains-mono
            nerd-fonts.symbols-only
            terminus_font
          ];
        in
        baselineFonts
        ++ (with pkgs; [
          atkinson-hyperlegible-mono
          atkinson-hyperlegible-next
          fira
          gentium
          libre-baskerville
          newcomputermodern
          stix-two
        ])
      );
    }
  );

  aspects.graphical.home = {
    fonts.fontconfig.enable = true;
  };
}
