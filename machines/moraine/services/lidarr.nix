{ config, ... }:
{
  services.lidarr = {
    enable = true;
    openFirewall = true;
    dataDir = "/mnt/local/Media/Music";
  };
}
