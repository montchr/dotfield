{ flake, pkgs, ... }:
{
  imports = [
    flake.config.aspects.theme.home

    ../profiles/graphical/common.nix

    ../profiles/graphical/applications/chromium.nix
    ../profiles/graphical/applications/firefox/default.nix
    ../profiles/graphical/applications/ghostty.nix
    #    ../profiles/graphical/applications/kitty/default.nix

    ../profiles/hardware/keyboard.nix

    ../profiles/multimedia/default.nix

  ];

  programs.nnn.extraPackages = [
    pkgs.imagemagick
    pkgs.ffmpeg
    pkgs.ffmpegthumbnailer
    pkgs.fontpreview
    pkgs.poppler # pdf rendering
    pkgs.viu
    pkgs.w3m # text-mode web browser
  ];
}
