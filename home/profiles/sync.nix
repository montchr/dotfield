{ pkgs, ... }:
{
  # services.git-sync.enable = true;
  # services.git-sync.repositories = {};
  services.syncthing.enable = true;

  home.packages = [ pkgs.nextcloud-client ];
}
