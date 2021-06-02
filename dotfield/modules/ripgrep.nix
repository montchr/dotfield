{ pkgs, lib, config, ... }:

let
  cfg = config.my.modules.ripgrep;
in {
  options = with lib; {
    my.modules.ripgrep = {
      enable = mkEnableOption ''
        Whether to enable ripgrep module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable {
      my = {
        env = { RIPGREP_CONFIG_PATH = "$XDG_CONFIG_HOME/ripgrep/config"; };

        user = { packages = with pkgs; [ ripgrep ]; };

        hm.configFile = {
          "ripgrep/config".text = "";
        };
      };
    };
}
