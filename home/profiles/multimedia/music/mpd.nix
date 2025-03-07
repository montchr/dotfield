{ config, ... }:
let
  beetsCfg = config.programs.beets;
in
{
  imports = [ ./playerctl.nix ];

  services.mpd = {
    enable = true;
    musicDirectory = beetsCfg.settings.directory;
    playlistDirectory = "${config.xdg.userDirs.music}/playlists";
    network.startWhenNeeded = true;
  };

  programs.beets.mpdIntegration = {
    enableStats = true;
    enableUpdate = true;
  };

  programs.ncmpcpp.enable = true;
}
