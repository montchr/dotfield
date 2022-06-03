{
  config,
  lib,
  pkgs,
  ...
}:
{
  # Prevent stupid boot delays waiting for internet.
  # https://discourse.nixos.org/t/boot-faster-by-disabling-udev-settle-and-nm-wait-online/6339
  systemd.services.systemd-udev-settle.enable = false;
  systemd.services.NetworkManager-wait-online.enable = false;
}
