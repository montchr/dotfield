{
  config,
  lib,
  pkgs,
  ...
}: {
  hardware.video.hidpi.enable = true;
  # N.B. Should be a multiple of 8/16/32 (which one is most accurate, idk)
  services.xserver.dpi = lib.mkDefault 176;
}
