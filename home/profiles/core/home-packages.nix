{
  pkgs,
  packages,
  ...
}: {
  home.packages = [
    ##: color utils
    pkgs.colorpanes #  <- print panes in the 8 bright terminal colors with shadows of the respective darker color
    pkgs.sanctity #    <- ruSt ANsi16 Color Test utIliTY
    pkgs.pastel #      <- generate, analyze, convert and manipulate colors
    (pkgs.writeShellApplication {
      name = "color-panic";
      runtimeInputs = [pkgs.colorpanes];
      text = ''
        colorpanes --captions --height 38 --width 24
      '';
    })

    ##: help me...
    pkgs.fx #        <- interactive terminal json viewer                  => <https://github.com/antonmedv/fx>
    pkgs.glow #      <- charmbracelet's markdown cli renderer
    pkgs.moreutils # <- almost core utils
    pkgs.treefmt #   <- one cli to format the code tree
    pkgs.watchexec # <- run commands when files change

    ##: [lukewarm]
    pkgs.grex #      <- generate regexps from user-provided test cases
    pkgs.lf #        <- yet another ranger-like file manager tui          => <https://github.com/gokcehan/lf/wiki/>

    ##: [TODO]
    pkgs.doggo #     <- command-line DNS client for humans                => <https://github.com/mr-karan/doggo>
    pkgs.duf #       <- better du/df alternative                          => <https://github.com/muesli/duf/>
    pkgs.fortune #   <- display pseudorandom quotations
    pkgs.hexyl #     <- command-line hex viewer
    pkgs.mprocs #    <- run multiple commands in parallel                 => <https://github.com/pvolok/mprocs>
    pkgs.ouch #      <- Obvious Unified Compression Helper                => <https://github.com/ouch-org/ouch>

    packages.ddi #   <- "nice dd setup for most cases"  => <https://git.sr.ht/~rycee/configurations/tree/1af2ef3d4c8778b0fb2b12934d3a3f1766ce1d9f/item/user/common.nix#L62-66>
  ];

  programs.pandoc.enable = true;
}
