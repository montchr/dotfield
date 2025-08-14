{
  dotfield.features.workstation.home =
    { pkgs, ... }:
    {
      programs.mpv.scripts = with pkgs.mpvScripts; [
        thumbnail # show thumbnail in seekbar
        mpv-playlistmanager
      ];
    };
}
