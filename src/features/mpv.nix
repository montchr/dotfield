{
  aspects.graphical.home =
    { pkgs, ... }:
    {
      programs.mpv = {
        enable = true;
        scripts = with pkgs.mpvScripts; [
          thumbnail
          mpv-playlistmanager
        ];
        config = {
          cache-default = 4000000;
          gpu-context = "wayland";
        };
      };
    };
}
