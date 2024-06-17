{
  lib,
  config,
  flake,
  ...
}:
let
  inherit (flake.inputs) nix-index-database;
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
in
{
  imports = [
    nix-index-database.hmModules.nix-index

    ./bat.nix
    ./home-packages.nix
    ./tealdeer.nix
  ];

  ### home-manager setup
  programs.home-manager.enable = true;
  manual.json.enable = true;
  news.display = "show";
  xdg.enable = true;

  ### shells
  programs.bash.enable = true;
  programs.zsh.enable = true;

  ### essential tools
  programs.jq.enable = true;
  programs.man.enable = true;
  # N.B. This can slow down builds, but enables more manpage integrations
  # across various tools.  See the home-manager manual for more info.
  programs.man.generateCaches = true;

  # Nix-oriented package search tool and `command-not-found` replacement.
  #
  # `nix-index` is useful in itself, but fish shell *needs* it, as
  # `command-not-found` simply spits out errors.
  #
  # <https://github.com/nix-community/nix-index>
  #
  programs.command-not-found.enable = false;
  # FIXME: the shell integration script for nix-index is probably sourced twice
  # when home-manager is loaded as a system module
  programs.nix-index.enable = true;
  programs.nix-index.symlinkToCacheHome = true;
  programs.nix-index-database.comma.enable = true;

  # User-defined executables should always be prioritized in $PATH.
  # TODO: double-check
  # FIXME: prob duplicated on nix-darwin
  home.sessionPath = lib.mkBefore (lib.singleton binHome);

  home.sessionVariables = {
    "EDITOR" = lib.mkDefault "vim";
    "LESSHISTFILE" = "${xdg.stateHome}/lesshst";
    "XDG_BIN_HOME" = binHome;
  };
}
