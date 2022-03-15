{
  pkgs,
  lib,
  config,
  ...
}: let
  configDir = "${config.dotfield.configDir}/hammerspoon";
in {
  homebrew.casks = ["hammerspoon"];

  # Point Hammerspoon to its init file.
  # https://github.com/Hammerspoon/hammerspoon/pull/582
  system.activationScripts.postUserActivation.text = ''
    defaults write org.hammerspoon.Hammerspoon MJConfigFile "${configDir}/init.lua"
  '';
}
