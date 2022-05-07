{
  config,
  lib,
  pkgs,
  ...
}: let
  envExtra = (import ../env-init.sh.nix);
  shellAliases = (import ../aliases.nix);
  shellGlobalAliases = (import ../abbrs.nix);
in {
  imports = [../common.nix];

  home.packages = with pkgs; [
    zsh
  ];

  programs.starship.enableZshIntegration = false;

  programs.zsh = {
    inherit
      envExtra
      shellAliases
      shellGlobalAliases;

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

    plugins = with pkgs; [
      # {
      #   name = "nix-zsh-complete.zsh";
      #   src = zsh-complete;
      #   file = "_nix";
      # }
      {
        name = "powerlevel10k";
        src = zsh-powerlevel10k;
        file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
      }
      {
        name = "powerlevel10k-config";
        src = lib.cleanSource ./p10k-config;
        file = "p10k.zsh";
      }
    ];

    initExtraFirst= ''
      if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
        source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
      fi
    '';

    initExtraBeforeCompInit = ''
      function md() { [[ $# == 1 ]] && mkdir -p -- "$1" && cd -- "$1" }
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
