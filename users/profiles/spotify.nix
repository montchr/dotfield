{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [spotify spotify-tui];
}
