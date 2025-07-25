{
  config,
  lib,
  pkgs,
  ...
}:
{
  environment.variables = {
    EDITOR = "vim";
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";

    # Although it points to a commonly-used path for user-owned executables,
    # $XDG_BIN_HOME is a non-standard environment variable. It is not part of
    # the XDG Base Directory Specification.
    # https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
    XDG_BIN_HOME = "$HOME/.local/bin";
  };

  environment.shells = [
    pkgs.bashInteractive
    pkgs.fish
    pkgs.zsh
  ];

  # Install completions for system packages.
  environment.pathsToLink = [
    "/share/bash-completion"
  ]
  # FIXME: figure out how to enable this without making all system rebuilds take forever
  # ++ (lib.optional config.programs.fish.enable "/share/fish")
  ++ (lib.optional config.programs.zsh.enable "/share/zsh");
}
