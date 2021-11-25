{ config, lib, pkgs, ... }:
let
  configDir = "${config.dotfield.configDir}/darwin";
in
{
  imports = [
    ./hammerspoon.nix
    ./yabai.nix
  ];

  my.user.packages = with pkgs; [
    (writeScriptBin "toggle-dark-mode"
      (builtins.readFile "${configDir}/bin/toggle-dark-mode"))
  ];
}
