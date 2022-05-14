{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [./xserver.nix];

  environment.systemPackages = with pkgs; [
    ## common utils
    qbittorrent
    simplescreenrecorder
    zip
  ];

  networking.networkmanager.enable = true;
  # TODO: [[id:5525264c-a043-4a88-8f26-244f5440e926][READ: simple linux networking with iwd]]
  # networking.networkmanager.wifi.backend = "iwd";

  # Prevent stupid boot delays waiting for internet.
  # https://discourse.nixos.org/t/boot-faster-by-disabling-udev-settle-and-nm-wait-online/6339
  systemd.services.systemd-udev-settle.enable = false;
  systemd.services.NetworkManager-wait-online.enable = false;

  # not everything can be free unless you're rms
  hardware.enableRedistributableFirmware = true;

  # fwupd :: a DBus service that allows applications to update firmware.
  services.fwupd.enable = true;

}

## sources:
#
# https://github.com/srid/nixos-config/blob/master/nixos/desktopish/default.nix
#
## ideas:
#
# elaborate pipewire configuration: https://github.com/balsoft/nixos-config/tree/b5ed51152f96225c0bb14482bdb3022b9c979679/profiles/sound.nix
