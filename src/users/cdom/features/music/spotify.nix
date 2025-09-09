{
  aspects.graphical.home =
    { pkgs, ... }:
    let
      tomlFormat = pkgs.formats.toml { };
    in
    {
      # <https://github.com/aome510/spotify-player/blob/master/docs/config.md#general>
      xdg.configFile."spotify-player/app.toml".source = tomlFormat.generate "spotify-player-app-config" {
        copy_command = "wl-copy";

        # <https://developer.spotify.com/dashboard/>
        client_id = "a51489a80c22465482734244dc626132";

        # Must match the callback URL specified in Spotify API configuration.
        client_port = 8080;
      };
    };
}
