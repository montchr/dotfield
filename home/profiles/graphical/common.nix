{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (config.home) homeDirectory;
in
{
  imports = [
    ./__gtk.nix
    ./__handlr.nix
    ./__mimeapps.nix

    ./applications/zathura.nix

    ../hardware/mouse.nix
  ];

  xdg = {
    userDirs = {
      enable = true;
      createDirectories = true;
      extraConfig = {
        # TODO: somehow share this value with home-manager git-sync?
        XDG_PROJECTS_DIR = homeDirectory + "/Projects";
        XDG_MAIL_DIR = "${homeDirectory}/Mail";
      };
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

  home.packages = [
    pkgs.dex # helper for working with xdg desktop entries
    pkgs.mediainfo
    pkgs.thunderbird-latest
    pkgs.ydotool # command-line automation tool
  ];

  fonts.fontconfig.enable = true;
}
