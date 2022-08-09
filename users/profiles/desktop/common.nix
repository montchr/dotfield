moduleArgs @ {
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isLinux;
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

    # TODO
    # xdg.desktopEntries = ...
    # xdg.mime = ...
    # xdg.mimeApps = ...
  }
