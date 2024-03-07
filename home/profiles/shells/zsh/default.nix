{ config, lib, ... }:
let
  inherit (config) xdg;
  hmLib = config.lib;
  cfg = config.programs.zsh;
  l = import ./lib.nix { inherit lib; };

  dotfieldDir = config.home.sessionVariables."DOTFIELD_DIR";

  zshDir = "$HOME/" + cfg.dotDir;
  DOTFIELD_USER_ZDOTDIR = "${dotfieldDir}/users/cdom/config/zsh";
  ZSH_CACHE = "${xdg.cacheHome}/zsh";
  ZSH_DATA = "${xdg.dataHome}/zsh";
in
{
  imports = [ ../common.nix ];

  home.extraOutputsToInstall = [ "/share/zsh" ];

  # Clear cached/compiled files on activation.
  home.activation.zshPurgeCaches = hmLib.dag.entryAfter [ "writeBoundary" ] ''
    $DRY_RUN_CMD rm -f $VERBOSE_ARG \
      ${zshDir}/*.zwc
    $DRY_RUN_CMD rm -rf $VERBOSE_ARG \
      ${ZSH_CACHE}
  '';

  programs.zsh = {
    enable = true;
    dotDir = ".config/zsh";
    history.path = "${ZSH_DATA}/history";

    # zsh-autosuggestions is loaded manually below to ensure proper load order.
    enableAutosuggestions = false;
    # zsh-autocomplete requires removal of any calls to compinit in zsh config.
    enableCompletion = false;

    initExtraFirst = l.mkInitProfiler ''
      ## Initialise the builtin profiler -- run `zprof` to read results
      zmodload zsh/zprof
    '';

    initExtra = l.mkInitUserConfig ''
      . "${DOTFIELD_USER_ZDOTDIR}/main.zsh"
    '';

    sessionVariables = {
      inherit DOTFIELD_USER_ZDOTDIR ZSH_CACHE ZSH_DATA;
    };
  };
}
## References:
# - <https://github.com/zsh-users/zsh-completions/blob/master/zsh-completions-howto.org>
