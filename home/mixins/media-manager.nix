{ pkgs, ... }:
{
  imports = [
    ../profiles/yt-dlp.nix
    ../profiles/media/music.nix
    ../profiles/media/beets/default.nix
  ];

  home.packages = [ pkgs.intermodal ];
}
