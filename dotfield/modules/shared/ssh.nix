{ pkgs, lib, config, ... }:

let

  dotfield = config.dotfield;

  cfg = config.my.modules.ssh;
  # cfgDir = "${config.dotfield.configDir}/ssh";

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
      # TODO: option doesn't exist! maybe it's NixOS only.
      # programs.ssh.startAgent = true;

      my.hm.file = {
        ".ssh/config" = {
          # TODO: make a config, and use it.
          # source = "${dotfield.configDir}/ssh/config";
          text = "";
        };
      };
    };
}
