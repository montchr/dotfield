{
  imports = [
    ./media-client.nix

    ./music/default.nix
    ./music/spotify.nix

    ./video/jellyfin-client.nix
    ./video/mpv.nix
    ./video/yt-dlp.nix
  ];
}
