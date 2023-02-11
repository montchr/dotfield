{
  pkgs,
  packages,
  ...
}: {
  home.packages = [
    packages.ddi
    pkgs.fx #        <- interactive terminal json viewer => <https://github.com/antonmedv/fx>
    pkgs.grex #      <- generate regexps from user-provided test cases
    pkgs.pastel #    <- generate, analyze, convert and manipulate colors
    pkgs.treefmt #   <- one cli to format the code tree
  ];

  programs.pandoc.enable = true;
}
