{ pkgs, lib, config, ... }:

let
  cfg = config.my.modules.hammerspoon;
  configDir = "${config.dotfield.configDir}/hammerspoon";
in {
  options = with lib; {
    my.modules.hammerspoon = {
      enable = mkEnableOption ''
        Whether to enable hammerspoon module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable {
      # Point Hammerspoon to its init file.
      # https://github.com/Hammerspoon/hammerspoon/pull/582
      system.activationScripts.postUserActivation.text = ''
        defaults write org.hammerspoon.Hammerspoon MJConfigFile "${configDir}/init.lua"
      '';
    };
}
