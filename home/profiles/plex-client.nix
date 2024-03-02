moduleArgs @ {
  config,
  lib,
  ...
}: let
  inherit (config.dotfield.features) hasWayland;
  hasNvidia = moduleArgs.osConfig.dotfield.features.hasNvidia or false;
in {
  # https://aur.archlinux.org/packages/plex-htpc#comment-854436
  xdg.dataFile."plex/mpv.conf".text = ''
    cache-default=4000000
    ${lib.optionalString hasNvidia "hwdec=vdpau"}
    ${lib.optionalString hasWayland "gpu-context=wayland"}
  '';
}
