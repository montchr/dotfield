{
  flake,
  pkgs,
  ...
}:
{
  imports = [
    ./mpd.nix
    ./playerctl.nix
  ];

  home.packages = [
    pkgs.quodlibet-full

    flake.perSystem.packages.scotty

    (pkgs.writeShellScriptBin "spotify-playlist-json-to-jspf" ''
      jq '.playlists.[] | {title: .name, date: .lastModifiedDate, track: (.items | map({title: .track.trackName, creator: .track.artistName, album: .track.albumName, identifier: [.track.trackUri]}))}' ./Playlist1.json
    '')
  ];
}
