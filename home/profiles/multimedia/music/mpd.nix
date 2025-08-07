{ pkgs, config, ... }:
let
  beetsCfg = config.programs.beets;
in
{
  imports = [ ./playerctl.nix ];

  services.mpd = {
    enable = true;
    musicDirectory = beetsCfg.settings.directory or "${config.home.homeDirectory}/Music/data";
    playlistDirectory = "${config.xdg.userDirs.music}/playlists";
    network.startWhenNeeded = true;
    network.listenAddress = "127.0.0.1";
    network.port = 6600;
  };

  programs.beets.mpdIntegration = {
    enableStats = true;
    enableUpdate = true;
  };

  programs.ncmpcpp.enable = true;
  services.amberol.enable = true;

  home.packages = with pkgs; [
    mpc
    ncmpc
    cantata # qt gui (ick)
    mmtc
    pms
  ];
}
