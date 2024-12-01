{ pkgs, ... }:
{
  imports = [
    ../profiles/multimedia/default.nix
    ../profiles/multimedia/music/beets/default.nix
    ../profiles/multimedia/video/yt-dlp.nix
  ];

  home.packages = [ pkgs.intermodal ];
}
