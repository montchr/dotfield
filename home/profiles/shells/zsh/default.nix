# FIXME: startup time has slowed to a crawl since refactor to use rc.d
##: References:
# - <https://github.com/zsh-users/zsh-completions/blob/master/zsh-completions-howto.org>
{
  config,
  pkgs,
  inputs,
  packages,
  self,
  ...
}: let
  inherit (config) xdg;
in {
  imports = [../common.nix];

  home.extraOutputsToInstall = [
    "/share/zsh"
    # TODO: is this already implied by `/share/zsh`?
    "/share/zsh/site-functions"
  ];

  home.packages = [
    packages.zsh-completions
  ];

  # Initialized manually to support p10k instant prompt.
  # <https://github.com/romkatv/powerlevel10k#how-do-i-initialize-direnv-when-using-instant-prompt>
  programs.direnv.enableZshIntegration = false;

  # Starship is notoriously slow.
  # <https://github.com/romkatv/zsh-bench/#prompt>
  # Use a zsh-only prompt instead.
  programs.starship.enableZshIntegration = false;

  # TODO: disable starship -- it's extremely slow --
  programs.zsh = {
    enable = true;
    dotDir = ".config/zsh";
    # zsh-autosuggestions is loaded manually below to insure proper load order.
    enableAutosuggestions = false;
    # zsh-autocomplete requires removal of any calls to compinit in zsh config.
    enableCompletion = false;
    autocd = true;

    shellAliases = {
      # Make it easy to copy/paste script commands verbatim
      # FIXME: don't use <space> -- this results in excluding the command from history
      "$" = " ";
    };

    defaultKeymap = "emacs";

    initExtraFirst = ''
      ##: Initialise the builtin profiler -- run `zprof` to read results
      zmodload zsh/zprof

      ##: Ensure zgenom is available
      if ! [[ -d $ZGEN_DIR ]]; then
        echo "Installing jandamm/zgenom"
        git clone --depth 1 https://github.com/jandamm/zgenom.git $ZGEN_DIR
      fi

      ##: Initialise zgenom
      . $ZGEN_DIR/zgenom.zsh
      # Check for plugin and zgenom updates every 7 days
      # This does not increase the startup time.
      zgenom autoupdate

      ##: Initialise prompt and direnv
      # - <https://github.com/romkatv/powerlevel10k#how-do-i-initialize-direnv-when-using-instant-prompt>

      emulate zsh -c "$(direnv export zsh)"

      # Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
      # Initialization code that may require console input (password prompts, [y/n]
      # confirmations, etc.) must go above this block; everything else may go below.
      if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
        source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
      fi

      emulate zsh -c "$(direnv hook zsh)"
    '';

    history.path = "${xdg.dataHome}/zsh/history";
    history.expireDuplicatesFirst = true;
    history.extended = true;
    history.ignoreDups = true;
    history.ignoreSpace = true;
    history.save = 10000;
    history.share = true;
    history.size = 10000;

    initExtra = ''
      export DOTFIELD_USER_ZDOTDIR="$DOTFIELD_DIR/home/users/cdom/config/zsh"
      . $DOTFIELD_USER_ZDOTDIR/main.zsh

      [[ -f ''${DOTFIELD_USER_ZDOTDIR:-$ZDOTDIR}/.p10k.zsh ]] \
        && source ''${DOTFIELD_USER_ZDOTDIR:-$ZDOTDIR}/.p10k.zsh
    '';

    sessionVariables = {
      ZGEN_DIR = "$XDG_DATA_HOME/zgenom";
      ZSH_CACHE = "${xdg.cacheHome}/zsh";
      ZSH_DATA = "${xdg.dataHome}/zsh";
    };
  };
}
