{
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    grex #      <- generate regexps from user-provided test cases
    pastel #    <- generate, analyze, convert and manipulate colors
    treefmt #   <- one cli to format the code tree
  ];

  programs.pandoc.enable = true;
}
