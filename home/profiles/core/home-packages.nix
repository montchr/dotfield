{
  pkgs,
  packages,
  ...
}: {
  home.packages = [
    pkgs.atool #     <- archive command line helper        => <https://www.nongnu.org/atool/>
    pkgs.doggo #     <- command-line DNS client for humans => <https://github.com/mr-karan/doggo>
    pkgs.duf #       <- better du/df alternative           => <https://github.com/muesli/duf/>
    pkgs.entr #      <- run commands when files change
    pkgs.fortune #   <- display pseudorandom quotations
    pkgs.fx #        <- interactive terminal json viewer   => <https://github.com/antonmedv/fx>
    pkgs.glow #      <- charmbracelet's markdown cli renderer
    pkgs.grex #      <- generate regexps from user-provided test cases
    pkgs.hexyl #     <- command-line hex viewer
    pkgs.lf #        <- yet another ranger-like file manager tui   => <https://github.com/gokcehan/lf/wiki/>
    pkgs.pastel #    <- generate, analyze, convert and manipulate colors
    pkgs.treefmt #   <- one cli to format the code tree
    pkgs.moreutils # <- almost core utils

    packages.ddi #   <- "nice dd setup for most cases" => <https://git.sr.ht/~rycee/configurations/tree/1af2ef3d4c8778b0fb2b12934d3a3f1766ce1d9f/item/user/common.nix#L62-66>
  ];

  programs.pandoc.enable = true;
}
