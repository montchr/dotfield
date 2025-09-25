{
  aspects.graphical.nixos =
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

      security.polkit.enable = true;
      security.rtkit.enable = true;
      security.sudo.wheelNeedsPassword = false;

      programs.dconf.enable = true;

      services.accounts-daemon.enable = true;
      services.dbus.packages = [
        pkgs.gcr # GNOME Crypto Services
        pkgs.gnome-settings-daemon
      ];
      services.gvfs.enable = true;

      environment.systemPackages = with pkgs; [
        adwaita-icon-theme
        dconf-editor
        dconf2nix
        glib # provides gsettings
        gnome-backgrounds
        libnotify
        mesa-demos # provides mesa utilities e.g. glxinfo
        qt5.qtwayland
        wev
        wl-clipboard
        wtype # abandoned?
        ydotool
      ];

      documentation.info.enable = true;
      documentation.man.enable = true;

      users.groups = {
        input = { inherit (config.users.groups.wheel) members; };
        video = { inherit (config.users.groups.wheel) members; };
      };
    };

  aspects.graphical.home =
    { config, pkgs, ... }:
    {
      programs.zathura.enable = true;

      home.packages = [
        pkgs.dex # helper for working with xdg desktop entries
        pkgs.imagemagick
        pkgs.mediainfo
        pkgs.signal-desktop
        pkgs.thunderbird-latest
      ];

      xdg.userDirs = {
        enable = true;
        createDirectories = true;
        extraConfig = {
          XDG_MAIL_DIR = "${config.home.homeDirectory}/Mail";
          XDG_PROJECTS_DIR = config.home.homeDirectory + "/Projects";
        };
      };

      systemd.user.targets.tray = {
        Unit = {
          Description = "Home Manager System Tray";
          Requires = [ "graphical-session-pre.target" ];
          # Any service starting after tray.target also needs to start
          # after "graphical-session.target" to prevent cyclic dependency.
          After = [ "graphical-session.target" ];
          PartOf = [ "graphical-session.target" ];
        };
        Install.WantedBy = [ "graphical-session.target" ];
      };
    };
}
