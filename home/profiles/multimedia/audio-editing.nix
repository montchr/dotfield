{ pkgs, ... }:
{
  home.packages = with pkgs; [
    ffmpeg
    sox
    tenacity
  ];
}
