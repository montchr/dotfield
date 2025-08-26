{
  dotfield.users.cdom.aspects.workstation.home =
    { config, pkgs, ... }:
    let
      beetsCfg = config.programs.beets;
    in
    {
      services.mpd = {
        enable = true;
        playlistDirectory = "${config.xdg.userDirs.music}/playlists";
        network.startWhenNeeded = true;
        network.listenAddress = "127.0.0.1";
        network.port = 6600;
      };

      services.playerctld.enable = true;

      programs.beets.mpdIntegration = {
        enableStats = true;
        enableUpdate = true;
      };

      programs.ncmpcpp.enable = true;
      services.amberol.enable = true;

      home.packages = with pkgs; [
        mpc
        ncmpc
        mmtc
        pms
        playerctl
      ];
    };
}
