{
  aspects.graphical.home =
    { pkgs, config, ... }:
    let
      beetsCfg = config.programs.beets;
    in
    {
      services.mpd = {
        enable = true;
        musicDirectory = beetsCfg.settings.directory;
        playlistDirectory = "${config.xdg.userDirs.music}/playlists";
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
    };
}
