# TODO: configure syncthing dirs
{ pkgs, ... }:
{
  services.syncthing.enable = true;

  home.packages = [ pkgs.nextcloud-client ];
}
