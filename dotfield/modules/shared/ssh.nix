{ pkgs, lib, config, ... }:

let

  cfg = config.my.modules.ssh;
  cfgDir = "${config.my.dotfield.configDir}/ssh";

in {
  options = with lib; {
    my.modules.ssh = {
      enable = mkEnableOption ''
        Whether to enable ssh module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable {
      my.hm.file = {
        ".ssh/config" = { source = builtins.toPath /. "${cfgDir}/config"; };
      };
    };
}
