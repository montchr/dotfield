{ config, ... }:
let
  userConfigsBase = "${config.xdg.configHome}/dotfield/home/users/cdom/config";
  inherit (config.lib.file) mkOutOfStoreSymlink;
in
{
  programs.zellij.enable = true;
  xdg.configFile."zellij".source = mkOutOfStoreSymlink "${userConfigsBase}/zellij";
}
