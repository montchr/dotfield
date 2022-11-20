{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    grex #      <- generate regexps from user-provided test cases
    pastel #    <- generate, analyze, convert and manipulate colors
    treefmt #   <- one cli to format the code tree
  ];

  programs.bottom.enable = true;
  programs.exa.enable = true;
  programs.exa.enableAliases = true;
  programs.jq.enable = true;
  programs.less.enable = true;
  programs.man.enable = true;
  # N.B. This can slow down builds, but enables more manpage integrations
  # across various tools. See the home-manager manual for more info.
  programs.man.generateCaches = lib.mkDefault true;
  programs.nix-index.enable = true;
  programs.pandoc.enable = true;
  programs.zoxide.enable = true;
}
