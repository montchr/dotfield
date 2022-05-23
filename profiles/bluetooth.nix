{
  config,
  lib,
  pkgs,
  ...
}: {
  hardware.bluetooth.enable = true;
  hardware.bluetooth.package = pkgs.bluezFull;
}
