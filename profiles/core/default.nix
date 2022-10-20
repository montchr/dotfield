{
  config,
  lib,
  pkgs,
  ...
}: let
  l = lib // builtins;
in {
  imports = [
    ../../lib/system
    ./nix-config.nix
    ./system-packages.nix
  ];

  time.timeZone = l.mkDefault "America/New_York";

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

  environment.shells = with pkgs; [
    bashInteractive
    fish
    zsh
  ];

  # Install completions for system packages.
  environment.pathsToLink =
    (l.optional config.programs.fish.enable "/share/fish")
    ++ (l.optional config.programs.zsh.enable "/share/zsh");

  programs.zsh = {
    enable = l.mkDefault true;
    enableCompletion = true;
    shellInit = l.mkBefore ''
      export ZDOTDIR="$HOME/.config/zsh"
    '';
  };

  programs.fish.enable = l.mkDefault true;
}
