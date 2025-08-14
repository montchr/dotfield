{ lib, ... }:
{
  dotfield.features.workstation.home =
    { pkgs, ... }:
    let
      inherit (pkgs.stdenv.hostPlatform) isLinux system;
      tomlFormat = pkgs.formats.toml { };
    in
    {
      home.packages = [
        pkgs.spotify-player

        (pkgs.writeShellScriptBin "spotify-playlist-json-to-jspf" ''
          jq '.playlists.[] | {title: .name, date: .lastModifiedDate, track: (.items | map({title: .track.trackName, creator: .track.artistName, album: .track.albumName, identifier: [.track.trackUri]}))}' ./Playlist1.json
        '')
      ]
      # XXX: broken upstream
      ++ (lib.optional (system != "aarch64-linux") pkgs.spotify);

      # <https://github.com/aome510/spotify-player/blob/master/docs/config.md#general>
      xdg.configFile."spotify-player/app.toml".source =
        tomlFormat.generate "spotify-player-app-config" {

          # <https://developer.spotify.com/dashboard/>
          client_id = "a51489a80c22465482734244dc626132";

          # Must match the callback URL specified in Spotify API configuration.
          client_port = 8080;
        }
        // (lib.optionalAttrs isLinux { copy_command = "${pkgs.wl-clipboard}/bin/wl-copy"; });
    };
}
