{ config, ... }:
let
  userConfigsBase = "${config.xdg.configHome}/dotfield/home/users/cdom/config";
  inherit (config.lib.file) mkOutOfStoreSymlink;
in
# FIXME: broken, of course
# inherit (config.dotfield.paths.userDirs) configsPath;
{
  programs.zellij.enable = true;
  xdg.configFile."zellij".source = mkOutOfStoreSymlink "${userConfigsBase}/zellij";
}
