{
  config,
  lib,
  pkgs,
  ...
}: let
  l = lib // builtins;
in {
  imports = [
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

    variables = l.mkDefault {};

    shellInit = l.mkDefault "";
    loginShellInit = l.mkDefault "";
    interactiveShellInit = l.mkDefault "";

    # A prompt should never be initialised at the system-level because it will
    # need to be initialised a second time once the user's zsh configs load.
    promptInit = l.mkForce "";

    # Completion should never be initialised at the system-level because it will
    # need to be initialised a second time once the user's zsh configs load.
    enableCompletion = l.mkForce false;
    enableBashCompletion = l.mkForce false;

    # These UI-enhancement plugins come at an even higher performance cost than
    # completion and do not belong in system configuration at all.
    enableFzfCompletion = l.mkForce false;
    enableFzfGit = l.mkForce false;
    enableFzfHistory = l.mkForce false;
    enableSyntaxHighlighting = l.mkForce false;
  };

  programs.fish.enable = l.mkDefault true;
}
