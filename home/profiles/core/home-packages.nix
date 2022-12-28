{
  pkgs,
  packages,
  ...
}: {
  home.packages = [
    packages.ddi
    pkgs.grex #      <- generate regexps from user-provided test cases
    pkgs.just
    pkgs.pastel #    <- generate, analyze, convert and manipulate colors
    pkgs.treefmt #   <- one cli to format the code tree
  ];

  programs.pandoc.enable = true;
}
