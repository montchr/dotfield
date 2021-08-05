{ pkgs, lib, config, ... }:

let
  cfg = config.my.modules.tealdeer;
  configDir = "${config.dotfield.flkConfigDir}/tealdeer";
in {
  options = with lib; {
    my.modules.tealdeer = {
      enable = mkEnableOption ''
        Whether to enable tealdeer module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable {
      my = {
        env = {
          TEALDEER_CONFIG_DIR = "$XDG_CONFIG_HOME/tealdeer";
          TEALDEER_CACHE_DIR = "$XDG_CACHE_HOME/tealdeer";
        };

        user.packages = [ pkgs.tealdeer ];

        hm.configFile."tealdeer/config.toml".text = ''
          # https://dbrgn.github.io/tealdeer/config.html

          [display]
          use_pager = false
          compact = false

          [updates]
          auto_update = true
          auto_update_interval_hours = 24
        '';
      };

      system.activationScripts.postUserActivation.text = ''
        mkdir -p "${config.my.xdg.cache}/tealdeer"
      '';
    };

}
