{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = [pkgs.tealdeer];
  home.sessionVariables = {
    # FIXME: open PR on upstream hm to remove hard-coded config path on darwin
    TEALDEER_CONFIG_DIR = "${config.xdg.configHome}/tealdeer";
    # FIXME: open PR on upstream hm to add option to set this value (and ensure the directory exists)
    TEALDEER_CACHE_DIR = "${config.xdg.cacheHome}/tealdeer";
  };

  # https://dbrgn.github.io/tealdeer/config.html
  xdg.configFile."tealdeer/config.toml".text = ''
    [display]
    use_pager = false
    compact = false

    [updates]
    auto_update = true
    auto_update_interval_hours = 168 # 1 week
  '';

  home.activation.ensureTealdeerCacheDir = lib.hm.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD mkdir -p $VERBOSE_ARG "${config.home.sessionVariables.TEALDEER_CACHE_DIR}"
  '';
}
