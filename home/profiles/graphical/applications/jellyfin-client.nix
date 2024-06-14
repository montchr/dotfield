{ pkgs, ... }:
{
  imports = [ ../mpv.nix ];

  home.packages = [ pkgs.jellyfin-media-player ];
}
