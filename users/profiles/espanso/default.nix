{ config, lib, pkgs, ... }:

let
  configDir = "${config.dotfield.configDir}/espanso";
in

{
  my.hm.xdg.configFile."espanso" = {
    source = configDir;
    recursive = true;
  };
}
