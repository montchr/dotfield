{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.newsboat = {
    enable = true;
    autoReload = true;
  };
}
