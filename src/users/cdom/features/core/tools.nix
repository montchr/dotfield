{ moduleWithSystem, ... }:
{
  dotfield.nixos = moduleWithSystem (
    perSystem@{ config }:
    nixos@{ pkgs, ... }:
    {
      home.packages = [
        config.packages.realise-symlink

        pkgs.chawan # <- tui web browser
        pkgs.dua # better du, ay?
        pkgs.hexyl # <- hex viewer
        pkgs.moreutils # <- almost core utils
        pkgs.watchexec # <- run commands when files change
        pkgs.yazi # file manager
      ];
    }
  );

  dotfield.home = moduleWithSystem (
    perSystem@{ config }:
    nixos@{ pkgs, ... }:
    {
      home.packages = [
        pkgs.fx # <- interactive terminal json viewer                    => <https://github.com/antonmedv/fx>
        pkgs.glow # <- charmbracelet's markdown cli renderer
        pkgs.ouch # <- Obvious Unified Compression Helper                => <https://github.com/ouch-org/ouch>
        pkgs.pastel # <- generate, analyze, convert and manipulate colors

        ##: [evaluating]
        pkgs.joshuto # <- yet ANOTHER ranger-like file manager tui => <https://github.com/kamiyaa/joshuto>

        ##: [TODO]
        pkgs.duf # <- better du/df alternative                          => <https://github.com/muesli/duf/>

      ];
    }
  );
}
