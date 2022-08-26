moduleArgs @ {
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isLinux;
  inherit (config.home) homeDirectory;
in
  lib.mkIf isLinux {
    xdg.userDirs = {
      enable = true;
      createDirectories = true;
      extraConfig = {
        XDG_PROJECTS_DIR = homeDirectory + "/Developer";
        XDG_MAIL_DIR = config.accounts.email.maildirBasePath or "$HOME/Mail";
      };
    };

    qt.enable = true;

    programs.zathura.enable = true;

    # TODO
    # xdg.desktopEntries = ...
    # xdg.mime = ...
    # xdg.mimeApps = ...
  }
