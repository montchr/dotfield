{
  pkgs,
  packages,
  ...
}: {
  programs.htop.enable = true;
  programs.jq.enable = true;

  home.packages = [
    ##: good stuff
    pkgs.fx #        <- interactive terminal json viewer                  => <https://github.com/antonmedv/fx>
    pkgs.glow #      <- charmbracelet's markdown cli renderer
    pkgs.moreutils # <- almost core utils

    ##: [lukewarm]
    pkgs.grex #      <- generate regexps from user-provided test cases
    pkgs.lf #        <- yet another ranger-like file manager tui          => <https://github.com/gokcehan/lf/wiki/>

    ##: [TODO]
    pkgs.doggo #     <- command-line DNS client for humans                => <https://github.com/mr-karan/doggo>
    pkgs.duf #       <- better du/df alternative                          => <https://github.com/muesli/duf/>
    pkgs.fortune #   <- display pseudorandom quotations
    pkgs.hexyl #     <- command-line hex viewer
    pkgs.ouch #      <- Obvious Unified Compression Helper                => <https://github.com/ouch-org/ouch>

    packages.ddi #   <- "nice dd setup for most cases"  => <https://git.sr.ht/~rycee/configurations/tree/1af2ef3d4c8778b0fb2b12934d3a3f1766ce1d9f/item/user/common.nix#L62-66>
  ];
}
