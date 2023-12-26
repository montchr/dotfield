moduleArgs @ {
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.dotfield.features) hasWayland;
  isGnomeDesktop = moduleArgs.osConfig.services.xserver.desktopManager.gnome.enable or false;
  hasNvidia = moduleArgs.osConfig.dotfield.features.hasNvidia or false;
in {
  programs.mpv = {
    enable = true;

    scripts = with pkgs.mpvScripts;
      [
        autoload # autoload playlist entries before/after current file
        thumbnail # show thumbnail in seekbar
        mpv-playlistmanager
      ]
      # prevent screen blanking in GNOME
      ++ lib.optional isGnomeDesktop inhibit-gnome;

    config = lib.mkMerge [
      {
        # FIXME: doesn't belong here...?
        ytdl-format = "bestvideo+bestaudio";
        cache-default = 4000000;
      }
      (lib.optionalAttrs hasNvidia {
        hwdec = "vdpau";
      })
      (lib.optionalAttrs hasWayland {
        gpu-context = "wayland";
      })
    ];
  };
}
