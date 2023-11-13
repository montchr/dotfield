# NOTE: As of 2023-11-12, this zgenom-based configuration does not work properly.
#       It is only preserved as a backup for now.
{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config) lib xdg;
  inherit (config.xdg) cacheHome;

  l = import ./lib.nix {inherit lib;};

  cfg = config.programs.zsh;
  dotfieldDir = config.home.sessionVariables."DOTFIELD_DIR";

  zshDir = "$HOME/" + cfg.dotDir;
  zgenSrcDir = "${xdg.dataHome}/zgenom";
  zgenCacheDir = "${xdg.cacheHome}/zgenom";

  DOTFIELD_USER_ZDOTDIR = "${dotfieldDir}/users/cdom/config/zsh";
  ZSH_CACHE = "${xdg.cacheHome}/zsh";
  ZSH_DATA = "${xdg.dataHome}/zsh";
in {
  imports = [
    ../common.nix

    ./with-p10k-prompt.nix
    ./history.nix
  ];

  # Clear compiled/generated zgenom files on activation.
  home.activation.zgenomPurgeCaches = lib.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD rm -f $VERBOSE_ARG \
      ${zgenCacheDir}/init.zsh{,.zwc}
  '';

  programs.zsh = {
    initExtraFirst = l.mkInitPluginManager ''
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

    sessionVariables = {
      ZGEN_DIR = zgenCacheDir;
      ZGEN_SOURCE = zgenSrcDir;
    };
  };
}
## References:
# - <https://github.com/zsh-users/zsh-completions/blob/master/zsh-completions-howto.org>
