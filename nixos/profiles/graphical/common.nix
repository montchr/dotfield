{
  config,
  lib,
  pkgs,
  ...
}:
let
  isGnomeDesktop = config.services.desktopManager.gnome.enable;
in
{
  imports = [
    ./applications/default.nix
    ./fonts/default.nix

    ./__gnome-services.nix
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

  environment.systemPackages = with pkgs; [
    adwaita-icon-theme
    dconf-editor
    foot
    glib
    gnome-backgrounds
    libnotify
    qt5.qtwayland
    wev
    wl-clipboard
    wtype # abandoned?
    ydotool
  ];

  documentation.info.enable = true;
  documentation.man.enable = true;
}
