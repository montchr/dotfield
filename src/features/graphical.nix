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
      # Sushi is a quick file previewer like Quick Look on macOS.
      services.gnome.sushi.enable = true;
      services.gvfs.enable = true;

      environment.systemPackages = with pkgs; [
        adwaita-icon-theme
        dconf-editor
        dconf2nix
        drm_info
        file-roller
        foot
        glib # provides gsettings
        gnome-backgrounds
        libnotify
        loupe
        mesa-demos # provides mesa utilities e.g. glxinfo
        qt5.qtwayland
        nautilus
        wev
        wl-clipboard
        wlr-randr
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
        pkgs.chafa # terminal image previewer
        pkgs.dex # helper for working with xdg desktop entries
        pkgs.imagemagick
        pkgs.mediainfo
        pkgs.signal-desktop
        pkgs.thunderbird-latest
        pkgs.trash-cli
      ];

      xdg.userDirs = {
        enable = true;
        createDirectories = true;
        setSessionVariables = true;
        extraConfig = {
          "MAIL" = "${config.home.homeDirectory}/Mail";
          "PROJECTS" = config.home.homeDirectory + "/Projects";
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
