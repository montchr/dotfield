moduleArgs @ {
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.home) username;
  inherit (config.lib) dotfield;
in {
  home.packages = with pkgs; [
    ## === Sysadmin ===

    du-dust #   <- Like du but more intuitive.
    entr #      <- Run arbitrary commands when files change
    lnav #      <- Log file navigator
    # FIXME: marked as broken
    # procs #     <- A modern replacement for ps.

    ## === `bat` and friends ===
    # A cat(1) clone with wings.

    bat

    # Bash scripts that integrate bat with various command line tools.
    # https://github.com/eth-p/bat-extras/
    bat-extras.batman #     <- Read system manual pages (man) using bat as the manual page formatter.
    bat-extras.batgrep #    <- Quickly search through and highlight files using ripgrep.
    bat-extras.batdiff #    <- Diff a file against the current git index, or display the diff between two files.
    bat-extras.batwatch #   <- Watch for changes in files or command output, and print them with bat.
    bat-extras.prettybat #  <- Pretty-print source code and highlight it with bat.

    grex #      <- Generate regexps from user-provided test cases.
    httpie #    <- Modern, user-friendly command-line HTTP client for the API era.
    pastel #    <- A command-line tool to generate, analyze, convert and manipulate colors
    tealdeer #  <- A very fast implementation of tldr in Rust.

    ## === Formatters ===

    treefmt # One CLI to format the code tree
  ];

  programs.bash.enable = true;
  programs.fish.enable = true;
  programs.zsh.enable = true;

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

  home.extraOutputsToInstall = ["/share/zsh"];

  home.sessionVariables = {
    DOTFIELD_DIR = dotfield.fsPath;

    # Default is "1". But when typeset in PragmataPro that leaves no space
    # between the icon and its filename.
    # FIXME: enable when pragpro enabled
    # EXA_ICON_SPACING = "2";

    Z_DATA = "$XDG_DATA_HOME/z";
    Z_OWNER = username;

    LESSHISTFILE = "$XDG_STATE_HOME/lesshst";

    # Docker
    DOCKER_CONFIG = "$XDG_CONFIG_HOME/docker";
    MACHINE_STORAGE_PATH = "$XDG_DATA_HOME/docker-machine";

    # Go
    GOPATH = "$XDG_DATA_HOME/go";

    # Rust
    CARGO_HOME = "$XDG_DATA_HOME/cargo";
    RUSTUP_HOME = "$XDG_DATA_HOME/rustup";

    # GNU screen
    SCREENRC = "$XDG_CONFIG_HOME/screen/screenrc";

    # wd
    # https://github.com/mfaerevaag/wd
    WD_CONFIG = "$XDG_CONFIG_HOME/wd/warprc";
  };

  home.stateVersion = lib.mkForce "22.05";
}
