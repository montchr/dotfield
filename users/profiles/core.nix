{
  config,
  lib,
  pkgs,
  ...
}: {
  lib.dotfield.userPath = "${config.xdg.configHome}/dotfield";
  lib.dotfield.whoami = rec {
    firstName = "Chris";
    lastName = "Montgomery";
    fullName = "${firstName} ${lastName}";
    email = "chris@cdom.io";
    githubUserName = "montchr";
    pgpPublicKey = "0x135EEDD0F71934F3";
  };

  home.sessionVariables.DOTFIELD_DIR = config.lib.dotfield.userPath;

  home.packages = with pkgs; [
    ## === Sysadmin ===

    du-dust # Like du but more intuitive.
    lnav # Log file navigator
    procs # A modern replacement for ps.

    ## === Utilities ===

    bat # A cat(1) clone with wings.
    tealdeer # A very fast implementation of tldr in Rust.
    grex # Generate regexps from user-provided test cases.
    httpie # Modern, user-friendly command-line HTTP client for the API era.
  ];

  programs.bat = {
    enable = true;
    config = {
      theme = "base16-256";
      map-syntax = [
        ".*ignore:Git Ignore"
        ".gitconfig.local:Git Config"
        "**/mx*:Bourne Again Shell (bash)"
        "**/completions/_*:Bourne Again Shell (bash)"
        ".vimrc.local:VimL"
        "vimrc:VimL"
      ];
    };
  };
  programs.bottom.enable = true;
  programs.nix-index.enable = true;
}
