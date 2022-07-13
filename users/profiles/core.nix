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
    xplr # Yet another file explorer.

    ## === Formatters ===

    treefmt # One CLI to format the code tree
  ];

  programs.bash.enable = true;
  # programs.fish.enable = true;
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
    DOTFIELD_DIR = config.lib.dotfield.userPath;

    # Default is "1". But when typeset in PragmataPro that leaves no space
    # between the icon and its filename.
    # EXA_ICON_SPACING = "2";

    Z_DATA = "$XDG_DATA_HOME/z";
    Z_OWNER = config.home.username;

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
