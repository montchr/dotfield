{
  config,
  lib,
  pkgs,
  ...
}:
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  home.packages = with pkgs; [
    spotify
    spotify-tui
  ];
}
