{ lib, pkgs, ... }:
{
  imports = [
    ./__gtk.nix
    ./__qt.nix
    ./__wallpaper.nix
    ./__xdg.nix
  ];

  documentation.info.enable = true;
  # NOTE: Force override <numtide/srvos>.
  documentation.man.enable = lib.mkForce true;

  services.xserver.enable = true;
  services.xserver.xkb.layout = "us";

  # <https://wiki.archlinux.org/title/Users_and_groups#Pre-systemd_groups>
  dotfield.guardian.extraGroups = [
    "audio"
    "input"
    "video"
  ];

  hardware.opengl = {
    enable = true;
    driSupport = true;
  };

  security.rtkit.enable = true;
  security.sudo.wheelNeedsPassword = false;

  # Hide cursor upon keystroke.
  # FIXME: "could not open display"
  # services.unclutter = {
  #   enable = true;
  #   keystroke = true;
  # };

  # Prevent stupid boot delays waiting for internet.
  # FIXME: this doesn't really seem to help much. dhcp still delays boot.
  # https://discourse.nixos.org/t/boot-faster-by-disabling-udev-settle-and-nm-wait-online/6339
  systemd.services.systemd-udev-settle.enable = false;
  systemd.services.NetworkManager-wait-online.enable = false;

  # # "Symlinks and syncs browser profile dirs to RAM thus reducing HDD/SDD calls
  # # and speeding-up browsers."
  # # <https://github.com/graysky2/profile-sync-daemon>
  # # <https://wiki.archlinux.org/title/Profile-sync-daemon>
  # services.psd.enable = true;
  # services.psd.resyncTimer = lib.mkDefault "10m";

  environment.systemPackages = [
    pkgs.wev # the Wayland Event Viewer
    pkgs.wl-clipboard
  ];
}
