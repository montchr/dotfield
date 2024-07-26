{ pkgs, ... }:
{
  imports = [ ../profiles/yt-dlp.nix ];

  home.packages = [ pkgs.intermodal ];
}
