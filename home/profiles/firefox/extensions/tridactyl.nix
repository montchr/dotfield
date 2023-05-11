{config, ...}: let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  userConfigsBase = "${config.xdg.configHome}/dotfield/home/users/cdom/config";
  # FIXME: broken, of course
  # inherit (config.dotfield.paths.userDirs) configsPath;
in {
  xdg.configFile."tridactyl".source =
    mkOutOfStoreSymlink "${userConfigsBase}/tridactyl";
}
