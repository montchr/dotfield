moduleArgs @ {
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isLinux;
  isNvidia = moduleArgs.osConfig.hardware.nvidia.modesetting.enable or false;
  isWayland = moduleArgs.osConfig.services.xserver.displayManager.gdm.wayland or false;
in
  lib.mkIf isLinux {
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
        (lib.mkIf isNvidia {
          hwdec = "vdpau";
        })
        (lib.mkIf isWayland {
          gpu-context = "wayland";
        })
      ];
    };

    # https://aur.archlinux.org/packages/plex-htpc#comment-854436
    xdg.dataFile."plex/mpv.conf".text = ''
      cache-default=4000000
      ${lib.optionalString isNvidia "hwdec=vdpau"}
      ${lib.optionalString isWayland "gpu-context=wayland"}
    '';

    # TODO
    # xdg.desktopEntries = ...
    # xdg.mime = ...
    # xdg.mimeApps = ...
  }
