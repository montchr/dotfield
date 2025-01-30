{
  config,
  flake,
  lib,
  pkgs,
  ...
}:
let
  isGnomeDesktop = config.services.xserver.desktopManager.gnome.enable;
in
{
  imports = [
    ./applications/default.nix
    ./fonts/default.nix

    ./__gnome-services.nix

    ../boot/systemd-boot.nix
  ];

  # <https://wiki.archlinux.org/title/Users_and_groups#Pre-systemd_groups>
  dotfield.guardian.extraGroups = [
    "audio"
    "input"
    "video"
  ];

  services.xserver.enable = true;
  services.xserver.xkb.layout = "us";
  hardware.graphics.enable = true;
  qt.enable = true;

  xdg = {
    mime.enable = true;
    icons.enable = true;
    portal.enable = true;
    portal.extraPortals = lib.optional (!isGnomeDesktop) pkgs.xdg-desktop-portal-gtk;
  };

  security.rtkit.enable = true;
  security.sudo.wheelNeedsPassword = false;

  programs.dconf.enable = true;
  programs.kdeconnect.enable = true;

  environment.systemPackages = with pkgs; [
    adwaita-icon-theme
    dconf-editor
    foot
    glib
    gnome-backgrounds
    qt5.qtwayland
    wev
    wl-clipboard
  ];

  # Prevent stupid boot delays waiting for internet.
  # FIXME: this doesn't really seem to help much. dhcp still delays boot.
  # https://discourse.nixos.org/t/boot-faster-by-disabling-udev-settle-and-nm-wait-online/6339
  systemd.services.systemd-udev-settle.enable = false;
  systemd.services.NetworkManager-wait-online.enable = false;

  documentation.info.enable = true;
  # HACK: Force override <numtide/srvos>.
  documentation.man.enable = lib.mkForce true;

}
