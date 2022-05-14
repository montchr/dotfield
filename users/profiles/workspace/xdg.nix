{config, lib, pkgs, ...}:

let
  inherit (config.home) homeDirectory;
in

{
  xdg.enable = true;
  xdg.userDirs.enable = true;
  systemd.user.tmpfiles.rules = [
    "D %h/tmp/downloads - - -"
    "D %h/tmp - - -"
  ];

  xdg.userDirs.desktop = "${homeDirectory}/usr/desktop";
  xdg.userDirs.documents = "${homeDirectory}/usr/docs";
  xdg.userDirs.download = "${homeDirectory}/tmp/downloads";
  xdg.userDirs.music = "${homeDirectory}/usr/music";
  xdg.userDirs.pictures = "${homeDirectory}/usr/images";
  xdg.userDirs.videos = "${homeDirectory}/usr/videos";
  xdg.userDirs.templates = "/tmp/seadome-templates";
  xdg.userDirs.publicShare = "/tmp/seadome-public-share";

  home.packages = with pkgs; [
    # xdg_utils
  ];
}

## sources:
# https://github.com/tudurom/dotfiles/blob/3506530a3e06fa78bdfc84c76d8c912f3fbd18c7/modules/desktop/dirs.nix
