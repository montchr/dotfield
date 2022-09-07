{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.lib) dotfield;
in {
  imports = [
    ../../lib/system
    ./nix-config.nix
    ./system-packages.nix
  ];

  # TODO: can this be merged with the 'dotfield' lib?
  # lib.dotfield = self.lib;

  time.timeZone = "America/New_York";

  environment.variables = {
    DOTFIELD_DIR = dotfield.fsPath;
    EDITOR = "vim";
    KERNEL_NAME =
      if pkgs.stdenv.hostPlatform.isDarwin
      then "darwin"
      else "linux";
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";
    ZDOTDIR = "$HOME/.config/zsh";

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
  environment.pathsToLink = [
    (lib.optionalString config.programs.fish.enable "/share/fish")
    (lib.optionalString config.programs.zsh.enable "/share/zsh")
  ];

  programs.zsh = {
    enable = lib.mkDefault true;
    enableCompletion = true;
  };

  programs.fish.enable = lib.mkDefault true;

  # This setting is enabled within the shared core profile because enabling it
  # for standalone home-manager configurations would disable hm's `nixpkgs.*`
  # options. When an hm config is built for a NixOS or nix-darwin system, we can
  # rely on the system's nixpkgs configuration.
  home-manager.useGlobalPkgs = true;
}
