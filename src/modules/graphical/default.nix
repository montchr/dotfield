{ self, lib, ... }:
{
  dotfield.modules.graphical.nixos =
    { config, pkgs, ... }:
    {
      imports = [ self.dotfield."boot/systemd-boot" ];

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
      security.polkit.enable = true;
      services.accounts-daemon.enable = true;
      services.dbus.packages = [
        pkgs.gcr
        pkgs.gnome-settings-daemon
      ];
      # Ref: <https://github.com/NixOS/nixpkgs/blob/3030f185ba6a4bf4f18b87f345f104e6a6961f34/nixos/modules/services/x11/desktop-managers/gnome.nix#L395>
      services.gvfs.enable = true;

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
      # HACK: Force override <numtide/srvos>.
      documentation.man.enable = lib.mkForce true;
    };
}
