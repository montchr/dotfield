{config, ...}: let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  configsDir = config.dotfield.paths.userDirs.configs;
in {
  programs.zellij.enable = true;
  xdg.configFile."zellij".source = mkOutOfStoreSymlink "${configsDir}/zellij";
}
