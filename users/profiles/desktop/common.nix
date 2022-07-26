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
  qt.platformTheme = "gnome";
  qt.style.package = pkgs.adwaita-qt;
  # FIXME: dark mode
  qt.style.name = "adwaita";

  programs.zathura.enable = true;

  # TODO
  # xdg.desktopEntries = ...
  # xdg.mime = ...
  # xdg.mimeApps = ...
}
