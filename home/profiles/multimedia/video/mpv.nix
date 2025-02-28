moduleArgs@{
  config,
  lib,
  pkgs,
  ...
}:
{
  programs.mpv = {
    enable = true;
    scripts = with pkgs.mpvScripts; [
      thumbnail # show thumbnail in seekbar
      mpv-playlistmanager
    ];
    config = {
      cache-default = 4000000;
      gpu-context = "wayland";
    };
  };
}
