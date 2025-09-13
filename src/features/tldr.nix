{ lib, ... }:
{
  aspects.core.home =
    { config, ... }:
    let
      inherit (config) xdg;
    in
    {
      programs.tealdeer.enable = true;
      programs.tealdeer.settings = {
        display = {
          use_pager = false;
          compact = false;
        };
        # Prevent activation failure during boot when using the NixOS module and
        # network is not yet available. Even if network were available, updating
        # this cache during boot is not appropriate.
        #
        # Note that this option will likely be removed in the near future:
        # <https://github.com/nix-community/home-manager/pull/5005>
        updateOnActivation = lib.mkForce false;
        updates = {
          auto_update = true;
          auto_update_interval_hours = 24 * 7;
        };
        directories.cache_dir = "${xdg.cacheHome}/tealdeer";
      };
    };
}
