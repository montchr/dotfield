{ config, lib, pkgs, ... }:
let
  configDir = "${config.dotfield.configDir}/karabiner";
in
{
  my.hm.xdg.configFile = {
    "karabiner/karabiner.json" = {
      source = "${configDir}/karabiner.json";
    };
  };
}
