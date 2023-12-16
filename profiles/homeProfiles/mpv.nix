moduleArgs @ {
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.dotfield.features) hasWayland;
  hasNvidia = moduleArgs.osConfig.dotfield.features.hasNvidia or false;
in {
  programs.mpv = {
    enable = true;
    package = pkgs.mpv.override {
      scripts = with pkgs.mpvScripts; [
        mpv-playlistmanager
        thumbfast
        uosc
      ];
    };
    config = lib.mkMerge [
      {
        ytdl-format = "bestvideo+bestaudio";
        cache-default = 4000000;
      }
      (lib.mkIf hasNvidia {
        hwdec = "vdpau";
      })
      (lib.mkIf hasWayland {
        gpu-context = "wayland";
      })
    ];
  };

  # https://aur.archlinux.org/packages/plex-htpc#comment-854436
  xdg.dataFile."plex/mpv.conf".text = ''
    cache-default=4000000
    ${lib.optionalString hasNvidia "hwdec=vdpau"}
    ${lib.optionalString hasWayland "gpu-context=wayland"}
  '';
}
