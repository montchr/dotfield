moduleArgs@{ lib, pkgs, ... }:
let
  hasNvidia = moduleArgs.osConfig.dotfield.features.hasNvidia or false;
in
{
  programs.mpv = {
    enable = true;
    scripts = with pkgs.mpvScripts; [
      autoload # autoload playlist entries before/after current file
      thumbnail # show thumbnail in seekbar
      mpv-playlistmanager
    ];
    config = lib.mkMerge [
      {
        gpu-context = "wayland";
        # FIXME: doesn't belong here...?
        ytdl-format = "bestvideo+bestaudio";
        cache-default = 4000000;
      }
      (lib.optionalAttrs hasNvidia { hwdec = "vdpau"; })
    ];
  };
}
