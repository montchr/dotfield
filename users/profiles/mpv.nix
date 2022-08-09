moduleArgs @ {
  config,
  lib,
  pkgs,
  ...
}: let
  isNvidia = moduleArgs.osConfig.hardware.nvidia.modesetting.enable or false;
  isWayland = moduleArgs.osConfig.services.xserver.displayManager.gdm.wayland or false;
in {
  programs.mpv = {
    enable = true;
    config = lib.mkMerge [
      {
        ytdl-format = "bestvideo+bestaudio";
        cache-default = 4000000;
      }
      (lib.mkIf isNvidia {
        hwdec = "vdpau";
      })
      (lib.mkIf isWayland {
        gpu-context = "wayland";
      })
    ];
  };

  # https://aur.archlinux.org/packages/plex-htpc#comment-854436
  xdg.dataFile."plex/mpv.conf".text = ''
    cache-default=4000000
    ${lib.optionalString isNvidia "hwdec=vdpau"}
    ${lib.optionalString isWayland "gpu-context=wayland"}
  '';
}
