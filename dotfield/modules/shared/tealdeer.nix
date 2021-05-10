{ pkgs, lib, config, ... }:

let
  cfg = config.my.modules.tealdeer;
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
      my.env = {
        TEALDEER_CONFIG_DIR = "${my.xdg.config}/tealdeer/config.toml";
        TEALDEER_CACHE_DIR = "${my.xdg.cache}/tealdeer";
      };

      my.user = { packages = with pkgs; [ tealdeer ]; };

      my.hm.configFile = {
        "tealdeer" = {
          source = "${dotfield.configDir}/tealdeer";
          recursive = true;
        };
      };
    };
}
