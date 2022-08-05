moduleArgs @ {
  config,
  lib,
  pkgs,
  ...
}:
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  xdg.userDirs = {
    enable = true;
    createDirectories = true;
    extraConfig = {
      XDG_MAIL_DIR = config.accounts.email.maildirBasePath or "$HOME/Mail";
      XDG_PROJECTS_DIR = "$HOME/Projects";
    };
  };

  qt.enable = true;
  # TODO: disabled while troubleshooting plex-htpc... 
  # qt.platformTheme = "gnome";
  # qt.style.package = pkgs.adwaita-qt;
  # qt.style.name = "adwaita";

  programs.zathura.enable = true;

  programs.mpv = {
    enable = true;
    config = lib.mkMerge [
      {
        ytdl-format = "bestvideo+bestaudio";
        cache-default = 4000000;
      }
      (lib.mkIf moduleArgs.osConfig.hardware.nvidia.modesetting.enable {
        hwdec = "vdpau";
      })
      (lib.mkIf (moduleArgs.osConfig.xserver.displayManager.gdm.wayland or false) {
        gpu-context = "wayland";
      })
    ];
  };

  # https://aur.archlinux.org/packages/plex-htpc#comment-854436
  # FIXME: assumes nvidia
  xdg.dataFile."plex/mpv.conf".text = ''
    hwdec=vdpau
    cache-default=4000000
  '';


  # TODO
  # xdg.desktopEntries = ...
  # xdg.mime = ...
  # xdg.mimeApps = ...
}
