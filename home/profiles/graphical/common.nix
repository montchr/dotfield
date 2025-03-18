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

    ./applications/calibre.nix
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

  home.packages = [
    pkgs.dex # helper for working with xdg desktop entries
    pkgs.mediainfo
    pkgs.thunderbird-latest
    pkgs.ydotool # command-line automation tool
  ];

  fonts.fontconfig.enable = true;
}
