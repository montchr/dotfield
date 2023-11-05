{
  lib,
  config,
  ...
}: let
  inherit (config) xdg;

  # Although it points to a commonly-used path for user-owned executables,
  # $XDG_BIN_HOME is a non-standard environment variable. It is not part of
  # the XDG Base Directory Specification.
  #
  # https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
  #
  # NOTE: This may also be set at the system level -- it is included again here
  # for standalone installation parity.
  binHome = "$HOME/.local/bin";
in {
  imports = [
    ./bat.nix
    ./home-packages.nix
    ./nixpkgs-config.nix
    ./tealdeer.nix
  ];

  ### home-manager setup
  programs.home-manager.enable = true;
  manual.json.enable = true;
  news.display = "show";
  xdg.enable = true;

  ### shells
  programs.bash.enable = true;
  programs.zsh.enable = lib.mkDefault true;

  ### essential tools
  programs.jq.enable = true;
  programs.man.enable = true;
  # N.B. This can slow down builds, but enables more manpage integrations
  # across various tools. See the home-manager manual for more info.
  programs.man.generateCaches = lib.mkDefault true;

  # Modern replacement for `command-not-found` with fish-shell support.
  programs.nix-index.enable = true;

  # User-defined executables should always be prioritized in $PATH.
  # TODO: double-check
  home.sessionPath = lib.mkBefore (lib.singleton binHome);

  home.sessionVariables = {
    LESSHISTFILE = "${xdg.stateHome}/lesshst";

    # Docker
    # NOTE: disabled so as not to interfere with overriding "docker" with other providers
    # TODO: move this to a docker-specific profile if ever needed.
    # DOCKER_CONFIG = "${xdg.configHome}/docker";
    # MACHINE_STORAGE_PATH = "${xdg.dataHome}/docker-machine";

    # Go
    GOPATH = "${xdg.dataHome}/go";

    # Rust
    CARGO_HOME = "${xdg.dataHome}/cargo";
    RUSTUP_HOME = "${xdg.dataHome}/rustup";

    # GNU screen
    SCREENRC = "${xdg.configHome}/screen/screenrc";

    # wd
    # https://github.com/mfaerevaag/wd
    WD_CONFIG = "${xdg.configHome}/wd/warprc";

    XDG_BIN_HOME = binHome;
  };
}
