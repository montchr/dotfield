{pkgs, ...}: {
  home.packages = with pkgs; [
    ffmpeg
    ttyd
    vhs
  ];
}
