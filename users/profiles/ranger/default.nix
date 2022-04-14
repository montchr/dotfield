{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.lib.file) mkOutOfStoreSymlink;
in {
  home.packages = with pkgs; [ranger];

  xdg.configFile = {
    "ranger/rc.conf".source = mkOutOfStoreSymlink (toString ./rc.conf);
    "ranger/rifle.conf".source = mkOutOfStoreSymlink (toString ./rifle.conf);
  };
}
