{ pkgs, ... }:
{
  imports = [
    ../profiles/multimedia/default.nix
    ../profiles/multimedia/audio-editing.nix
    ../profiles/multimedia/music/beets/default.nix
    ../profiles/multimedia/video/yt-dlp.nix
  ];

  home.packages = [
    # TODO: what this?
    pkgs.intermodal
  ];
}
