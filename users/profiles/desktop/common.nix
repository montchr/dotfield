{
  config,
  lib,
  pkgs,
  ...
}:
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  programs.zathura.enable = true;
  xsession.enable = true;
  qt.enable = true;
  xdg.userDirs.enable = true;

  # TODO
  # xdg.desktopEntries = ...
  # xdg.mime = ...
  # xdg.mimeApps = ...
}
