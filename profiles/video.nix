{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    mpv
    plex-media-player
  ];
}
