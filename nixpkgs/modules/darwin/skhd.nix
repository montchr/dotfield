{ config, home-manager, ... }:

let

  cfgDir = ${config.my.dotfield.configDir}/skhd;

in
{
  home-manager.services.skhd = {
    enable = true;
    skhdConfig = builtins.readFile ${cfgDir}/skhdrc;
  };
}
