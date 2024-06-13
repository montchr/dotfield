{ config, ... }:
let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  userConfigsBase = "${config.xdg.configHome}/dotfield/home/users/cdom/config";
in
{
  xdg.configFile."tridactyl".source = mkOutOfStoreSymlink "${userConfigsBase}/tridactyl";
}
