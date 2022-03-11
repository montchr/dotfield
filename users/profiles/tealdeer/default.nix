{ pkgs, lib, config, ... }:

let
  configDir = "${config.dotfield.configDir}/tealdeer";
in

{
  my.hm.home.sessionVariables = {
    TEALDEER_CONFIG_DIR = "$XDG_CONFIG_HOME/tealdeer";
    TEALDEER_CACHE_DIR = "$XDG_CACHE_HOME/tealdeer";
  };

  my.hm.xdg.configFile."tealdeer/config.toml".text = ''
    # ${config.my.nix_generated}
    # https://dbrgn.github.io/tealdeer/config.html

    [display]
    use_pager = false
    compact = false

    [updates]
    auto_update = true
    auto_update_interval_hours = 24
  '';

  system.activationScripts.postUserActivation.text = ''
    mkdir -p "${config.my.xdg.cache}/tealdeer"
  '';
}
