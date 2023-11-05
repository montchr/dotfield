{
  config,
  pkgs,
  ...
}: let
  inherit (config) lib xdg;
  inherit (config.xdg) cacheHome;
  cfg = config.programs.zsh;
  dotfieldDir = config.home.sessionVariables."DOTFIELD_DIR";

  zshDir = "$HOME/" + cfg.dotDir;
  zgenSrcDir = "${xdg.dataHome}/zgenom";
  zgenCacheDir = "${xdg.cacheHome}/zgenom";

  DOTFIELD_USER_ZDOTDIR = "${dotfieldDir}/users/cdom/config/zsh";
  ZSH_CACHE = "${xdg.cacheHome}/zsh";
  ZSH_DATA = "${xdg.dataHome}/zsh";
in {
  imports = [../common.nix];

  home.extraOutputsToInstall = [
    "/share/zsh"
    # TODO: is this already implied by `/share/zsh`?
    "/share/zsh/site-functions"
  ];

  # Clear cached/compiled files on activation.
  home.activation.zshPurgeCaches = lib.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD rm -f $VERBOSE_ARG \
      ${zshDir}/*.zwc
    $DRY_RUN_CMD rm -f $VERBOSE_ARG \
      ${zgenCacheDir}/init.zsh{,.zwc}
    $DRY_RUN_CMD rm -rf $VERBOSE_ARG \
      ${ZSH_CACHE}
  '';

  ##: Disable some integrations to support p10k prompt.
  # <https://github.com/romkatv/powerlevel10k#how-do-i-initialize-direnv-when-using-instant-prompt>
  programs.direnv.enableZshIntegration = false;
  # <https://github.com/romkatv/zsh-bench/#prompt>
  programs.starship.enableZshIntegration = false;

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

      ##: Initialise prompt and direnv
      # - <https://github.com/romkatv/powerlevel10k#how-do-i-initialize-direnv-when-using-instant-prompt>

      emulate zsh -c "$(direnv export zsh)"

      # Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
      # Initialization code that may require console input (password prompts, [y/n]
      # confirmations, etc.) must go above this block; everything else may go below.
      if [[ -r "${cacheHome}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
        source "${cacheHome}/p10k-instant-prompt-''${(%):-%n}.zsh"
      fi

      emulate zsh -c "$(direnv hook zsh)"

      ##: Ensure zgenom is available
      if ! [[ -d $ZGEN_SOURCE ]]; then
        echo "Installing jandamm/zgenom"
        git clone --depth 1 https://github.com/jandamm/zgenom.git $ZGEN_SOURCE
      fi

      ##: Initialise zgenom
      . $ZGEN_SOURCE/zgenom.zsh

      ##: Check for plugin and zgenom updates (default: every week).
      zgenom autoupdate
    '';

    history.path = "${ZSH_DATA}/history";
    history.expireDuplicatesFirst = true;
    history.extended = true;
    history.ignoreDups = true;
    history.ignoreSpace = true;
    history.save = 10000;
    history.share = true;
    history.size = 10000;

    initExtra = ''
      . ${DOTFIELD_USER_ZDOTDIR}/main.zsh
      [[ -f ${DOTFIELD_USER_ZDOTDIR}/.p10k.zsh ]] \
        && source ${DOTFIELD_USER_ZDOTDIR}/.p10k.zsh
    '';

    sessionVariables = {
      inherit DOTFIELD_USER_ZDOTDIR ZSH_CACHE ZSH_DATA;
      ZGEN_DIR = zgenCacheDir;
      ZGEN_SOURCE = zgenSrcDir;
    };
  };
}
##: References:
# - <https://github.com/zsh-users/zsh-completions/blob/master/zsh-completions-howto.org>

