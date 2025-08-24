flake@{ lib, ... }:
{
  dotfield.aspects.graphical.nixos =
    { config, pkgs, ... }:
    {
      services.xserver.enable = true;
      services.xserver.xkb.layout = "us";
      hardware.graphics.enable = true;
      qt.enable = true;

      xdg = {
        mime.enable = true;
        icons.enable = true;
        portal.enable = true;
        portal.extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
      };

      programs.dconf.enable = true;

      security.rtkit.enable = true;
      security.sudo.wheelNeedsPassword = false;
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
        eog
        ghostty
        glib
        gnome-backgrounds
        libnotify
        qt5.qtwayland
        qview
        wev
        wl-clipboard
        wtype # abandoned?
        ydotool
      ];

      # <https://wiki.archlinux.org/title/Users_and_groups#Pre-systemd_groups>
      users.groups.audio.members = config.users.groups.wheel.members;
      users.groups.input.members = config.users.groups.wheel.members;
      users.groups.video.members = config.users.groups.wheel.members;

      documentation.info.enable = true;
      # HACK: Force override <numtide/srvos>.
      documentation.man.enable = lib.mkForce true;
    };

  dotfield.aspects.graphical.home =
    { config, pkgs, ... }:
    {
      fonts.fontconfig.enable = true;

      programs.chromium = {
        enable = true;
        package = pkgs.chromium.override { enableWideVine = true; };
      };
      programs.zathura.enable = true;

      xdg.userDirs = {
        enable = true;
        createDirectories = true;
        extraConfig = {
          # TODO: somehow share this value with home-manager git-sync?
          XDG_PROJECTS_DIR = config.home.homeDirectory + "/Projects";
          XDG_MAIL_DIR = "${config.home.homeDirectory}/Mail";
        };
      };

      home.packages = [
        pkgs.dconf2nix # <https://github.com/gvolpe/dconf2nix>
        pkgs.dconf-editor
        pkgs.handlr-regex
        (pkgs.writeShellScriptBin "xterm" ''
          handlr launch x-scheme-handler/terminal -- "$@"
        '')
        (pkgs.writeShellScriptBin "xdg-open" ''
          handlr open "$@"
        '')
      ];
    };
}
