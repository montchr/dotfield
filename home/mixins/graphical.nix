{ pkgs, ... }:
{
  imports = [
    ../profiles/graphical/common.nix

    ../profiles/graphical/applications/chromium.nix
    ../profiles/graphical/applications/firefox/default.nix
    ../profiles/graphical/applications/floorp.nix
    ../profiles/graphical/applications/foot.nix
    ../profiles/graphical/applications/ghostty/default.nix
    #    ../profiles/graphical/applications/kitty/default.nix

    ../profiles/hardware/keyboard/default.nix
    ../profiles/multimedia/default.nix
    ../profiles/theme/default.nix
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
