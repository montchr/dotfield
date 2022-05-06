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
    enableCompletion = true;
    enableSyntaxHighlighting = true;
    enableAutosuggestions = true;
    autocd = true;

    history.path = "${config.xdg.dataHome}/zsh/history";
    history.expireDuplicatesFirst = true;
    history.extended = true;
    history.ignoreDups = true;

    prezto.enable = true;
    prezto.editor = {
      # Handled manually in aliases.
      dotExpansion = false;
    };
    prezto.terminal.autoTitle = true;
    # TODO: enable tmux
    # prezto.tmux.autoStartRemote = true;

    # initExtraFirst = ''
    #   # Load our custom z4h config directly
    #   source $DOTFIELD_DIR/config/zsh/main.zsh
    # '';

    sessionVariables = {
      ZSH_CACHE = "${config.xdg.cacheHome}/zsh";
      ZSH_DATA = "${config.xdg.dataHome}/zsh";
    };
  };
}
