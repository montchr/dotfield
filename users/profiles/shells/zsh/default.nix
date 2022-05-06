{
  config,
  lib,
  pkgs,
  ...
}: let
  envExtra = (import ../env-init.sh.nix);
  shellAliases =
    (import ../abbrs.nix)
    // (import ../aliases.nix);
in {
  imports = [../common.nix];

  home.packages = with pkgs; [
    zsh
  ];

  programs.zsh = {
    inherit envExtra shellAliases;

    enable = true;
    dotDir = ".config/zsh";
    history.path = "${config.xdg.dataHome}/zsh/history";
    history.extended = true;
    history.ignoreDups = true;

    # These are handled by z4h.
    enableCompletion = false;
    enableSyntaxHighlighting = false;

    envExtraFirst = ''
      ${builtins.readFile ./env-z4h.zsh}
    '';

    initExtraFirst = ''
      # Load our custom z4h config directly
      source $DOTFIELD_DIR/config/zsh/main.zsh
    '';

    sessionVariables = {
      ZSH_CACHE = "${config.xdg.cacheHome}/zsh";
      ZSH_DATA = "${config.xdg.dataHome}/zsh";
    };
  };
}
