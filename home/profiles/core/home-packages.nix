{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    ## === Sysadmin ===

    du-dust #   <- Like du but more intuitive.
    entr #      <- Run arbitrary commands when files change
    lnav #      <- Log file navigator
    # FIXME: marked as broken
    # procs #     <- A modern replacement for ps.

    grex #      <- Generate regexps from user-provided test cases.
    httpie #    <- Modern, user-friendly command-line HTTP client for the API era.
    pastel #    <- A command-line tool to generate, analyze, convert and manipulate colors
    tealdeer #  <- A very fast implementation of tldr in Rust.

    ## === Formatters ===

    treefmt # One CLI to format the code tree
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
  programs.tealdeer.enable = true;
  programs.zoxide.enable = true;
}
