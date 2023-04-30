{config, ...}: let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (config.dotfield.paths.userDirs) configsPath;
in {
  programs.zellij.enable = true;
  xdg.configFile."zellij".source = mkOutOfStoreSymlink "${configsPath}/zellij";
}
