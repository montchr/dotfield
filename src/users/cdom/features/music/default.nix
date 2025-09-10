{ moduleWithSystem, ... }:
{
  users.cdom.aspects.graphical.home = moduleWithSystem (
    perSystem@{ config }:
    { pkgs, ... }:
    {
      home.packages = [
        # XXX: broken (2025-07-24)
        # pkgs.quodlibet-full

        perSystem.config.packages.scotty

        (pkgs.writeShellScriptBin "spotify-playlist-json-to-jspf" ''
          jq '.playlists.[] | {title: .name, date: .lastModifiedDate, track: (.items | map({title: .track.trackName, creator: .track.artistName, album: .track.albumName, identifier: [.track.trackUri]}))}' ./Playlist1.json
        '')
      ];
    }
  );
}
