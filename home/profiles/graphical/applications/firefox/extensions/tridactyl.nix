{ config, ... }:
let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  userConfigsBase = "${config.xdg.configHome}/dotfield/home/users/cdom/config";
in
# FIXME: broken, of course
# inherit (config.dotfield.paths.userDirs) configsPath;
{
  xdg.configFile."tridactyl".source = mkOutOfStoreSymlink "${userConfigsBase}/tridactyl";
}
