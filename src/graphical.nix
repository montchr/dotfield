{ config, ... }:
let
  inherit (config.flake.modules) nixos;
in
{
  flake.modules.nixos.graphical =
    { config, pkgs, ... }:
    {
      imports = [

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
    };

  flake.modules.nixos.admin-user = {
    # <https://wiki.archlinux.org/title/Users_and_groups#Pre-systemd_groups>
    dotfield.guardian.extraGroups = [
      "input"
      "video"
    ];
  };
}
