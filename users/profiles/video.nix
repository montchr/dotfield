{
  config,
  lib,
  pkgs,
  ...
}: {
  home.package = with pkgs; [mpvc mpv-with-scripts];
  programs.mpv.enable = true;
}
