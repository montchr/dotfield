{ pkgs, lib, config, ... }:

let
  cfg = config.my.modules.tealdeer;
  configDir = "${config.dotfield.configDir}/tealdeer";
in {
  options = with lib; {
    my.modules.tealdeer = {
      enable = mkEnableOption ''
        Whether to enable tealdeer module
      '';
    };
  };

  config = with lib;
    let
      xdg = config.my.xdg;
    in
    mkIf cfg.enable {
      my = {
        env = {
          TEALDEER_CONFIG_DIR = "${xdg.config}/tealdeer";
          TEALDEER_CACHE_DIR = "${xdg.cache}/tealdeer";
        };

        user = { packages = with pkgs; [ tealdeer ]; };

        hm = {
          configFile = {
            "tealdeer" = {
              source = configDir;
              recursive = true;
            };
          };

        };
      };

      system.activationScripts.postUserActivation.text = ''
        mkdir -p ${xdg.cache}/tealdeer
      '';
    };

}
