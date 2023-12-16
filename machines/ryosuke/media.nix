{pkgs, ...}: {
  services.deluge.enable = true;
  environment.systemPackages = [
    pkgs.jellyfin-media-player
  ];
}
