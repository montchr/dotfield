{ moduleWithSystem, ... }:
{
  users.cdom.aspects.graphical.home = moduleWithSystem (
    perSystem@{ config }:
    { pkgs, ... }:
    let
      mkIosevkaSgrVariant =
        variant:
        pkgs.iosevka-bin.override {
          variant = "SGr-Iosevka${variant}";
        };
    in
    {
      home.packages = [
        (mkIosevkaSgrVariant "Term")
        perSystem.config.packages.astrata
        perSystem.config.packages.iosvmata-bin
        perSystem.config.packages.pragmasevka-bin
        pkgs.aporetic
        pkgs.bakoma_ttf
        # <https://software.sil.org/charis/>
        # <https://practicaltypography.com/charter.html>
        pkgs.charis-sil
        pkgs.commit-mono
        pkgs.departure-mono
      ];

      dconf.settings = {
        # FIXME: also set the equivalent settings for fontconfig
        "org/gnome/desktop/interface" = {
          font-antialiasing = "rgba";
          font-hinting = "slight";
          text-scaling-factor = 1.0;
        };
      };
    }
  );
}
