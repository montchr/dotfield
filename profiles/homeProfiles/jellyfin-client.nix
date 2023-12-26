{pkgs, ...}: {
  imports = [
    ./mpv.nix
  ];

  home.packages = [pkgs.jellyfin-media-player];

  services.jellyfin-mpv-shim.enable = true;
}
