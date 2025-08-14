{ moduleWithSystem, ... }:
{
  dotfield.features.archivist.nixos =
    { pkgs, ... }:
    {
      environment.systemPackages = [
        pkgs.ffmpeg
      ];
    };

  dotfield.features.workstation.home = moduleWithSystem (
    perSystem@{ config, ... }:
    { pkgs, ... }:
    {
      programs.mpv = {
        enable = true;
        scripts = with pkgs.mpvScripts; [
          thumbnail # show thumbnail in seekbar
        ];
        config = {
          cache-default = 4000000;
          gpu-context = "wayland";
        };
      };

      home.packages = [
        pkgs.sox
        pkgs.tenacity
        pkgs.jellyfin-media-player
        pkgs.quodlibet-full
        perSystem.config.packages.scotty
      ];
    }
  );
}
