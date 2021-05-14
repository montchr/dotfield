{ pkgs, lib, config, ... }:

let
  cfg = config.my.modules.tealdeer;
  configDir = "${config.dotfield.configDir}/tealdeer";
in
{
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
        mkdir -p "$XDG_CACHE_HOME/tealdeer"
      '';
    };

}
