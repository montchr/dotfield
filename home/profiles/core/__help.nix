{
  config,
  lib,
  ...
}: let
  inherit (config) xdg;
in {
  programs.command-not-found.enable = true;
  programs.info.enable = lib.mkDefault true;

  programs.man.enable = true;
  # N.B. This can slow down builds, but enables more manpage integrations
  # across various tools. See the home-manager manual for more info.
  # FIXME: enable only for workstations
  programs.man.generateCaches = lib.mkDefault true;

  programs.navi.enable = true;

  programs.tealdeer.enable = true;
  programs.tealdeer.settings = {
    display = {
      use_pager = false;
      compact = false;
    };
    updates = {
      auto_update = true;
      auto_update_interval_hours = 24 * 7;
    };
    directories.cache_dir = "${xdg.cacheHome}/tealdeer";
  };
}
