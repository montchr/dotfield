{ lib, pkgs, ... }:
let
  inherit (pkgs.stdenv.hostPlatform) isAarch64 isLinux;
  tomlFormat = pkgs.formats.toml { };
in
{
  home.packages = lib.optionals (isLinux && !isAarch64) [
    pkgs.spotify
    pkgs.spotify-player
  ];

  # <https://github.com/aome510/spotify-player/blob/master/docs/config.md#general>
  xdg.configFile."spotify-player/app.toml".source =
    tomlFormat.generate "spotify-player-app-config" {
      # <https://developer.spotify.com/dashboard/a51489a80c22465482734244dc626132/settings>
      client_id = "a51489a80c22465482734244dc626132";
      # Must match the callback URL specified in Spotify API configuration.
      client_port = 8080;
    }
    // (lib.optionalAttrs isLinux { copy_command = "${pkgs.wl-clipboard}/bin/wl-copy"; });
}
