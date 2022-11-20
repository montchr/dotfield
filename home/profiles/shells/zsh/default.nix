{
  config,
  pkgs,
  ...
}: let
  inherit (config) xdg;
  shellAliases = (import ../abbrs.nix) // (import ../aliases.nix);
in {
  imports = [../common.nix];

  home.extraOutputsToInstall = ["/share/zsh"];

  programs.starship.enableZshIntegration = true;

  programs.zsh = {
    inherit shellAliases;

    enable = true;
    dotDir = ".config/zsh";
    enableCompletion = true;
    enableSyntaxHighlighting = true;
    enableAutosuggestions = true;
    # enableVteIntegration = true;
    autocd = true;

    defaultKeymap = "viins";

    history.path = "${xdg.dataHome}/zsh/history";
    history.expireDuplicatesFirst = true;
    history.extended = true;
    history.ignoreDups = true;
    history.ignoreSpace = true;
    history.save = 10000;
    history.share = true;
    history.size = 10000;

    historySubstringSearch.enable = true;

    # plugins = with pkgs; [];

    initExtra = ''
      export DOTFIELD_USER_ZDOTDIR="$DOTFIELD_DIR/home/users/$USER/config/zsh"

      source $DOTFIELD_USER_ZDOTDIR/main.zsh
    '';

    sessionVariables = {
      ZSH_CACHE = "${xdg.cacheHome}/zsh";
      ZSH_DATA = "${xdg.dataHome}/zsh";
    };
  };
}
