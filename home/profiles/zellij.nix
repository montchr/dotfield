{config, ...}: let
  userConfigsBase = "${config.xdg.configHome}/dotfield/home/users/cdom/config";
  inherit (config.lib.file) mkOutOfStoreSymlink;
  # FIXME: broken, of course
  # inherit (config.dotfield.paths.userDirs) configsPath;
in {
  programs.zellij.enable = true;
  xdg.configFile."zellij".source =
    mkOutOfStoreSymlink "${userConfigsBase}/zellij";
}
