{
  config,
  lib,
  pkgs,
  ...
}: let
  envExtra = import ../env-init.sh.nix;
  shellAliases =
    (import ../abbrs.nix)
    // (import ../aliases.nix);
in {
  imports = [../common.nix];

  home.packages = with pkgs; [
    zsh
  ];

  programs.starship.enableZshIntegration = true;

  programs.zsh = {
    inherit
      envExtra
      shellAliases
      ;

    enable = true;
    dotDir = ".config/zsh";
    enableCompletion = true;
    enableSyntaxHighlighting = true;
    enableAutosuggestions = true;
    autocd = true;

    history.path = "${config.xdg.dataHome}/zsh/history";
    history.expireDuplicatesFirst = true;
    history.extended = true;
    history.ignoreDups = true;

    # plugins = with pkgs; [];

    initExtraFirst = ''
      # if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
      #   source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
      # fi
    '';

    initExtraBeforeCompInit = ''
      compdef _directories md
    '';

    initExtra = ''
      source $DOTFIELD_DIR/lib/color.sh
      source $DOTFIELD_DIR/config/zsh/functions.zsh
      source $DOTFIELD_DIR/config/zsh/options.zsh
    '';

    sessionVariables = {
      ZSH_CACHE = "${config.xdg.cacheHome}/zsh";
      ZSH_DATA = "${config.xdg.dataHome}/zsh";
    };
  };
}
