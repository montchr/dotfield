{ pkgs, lib, config, ... }:

let

  cfg = config.my.modules.direnv;
  # cfgDir = "${config.dotfield.configDir}/direnv";

in {
  options = with lib; {
    my.modules.direnv = {
      enable = mkEnableOption ''
        Whether to enable direnv module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable {
      my.user = { packages = with pkgs; [ direnv ]; };

      # my.hm.configFile = {
      #   "direnv" = {
      #     recursive = true;
      #     source = builtins.toPath /. cfgDir;
      #   };
      # };
    };
}
