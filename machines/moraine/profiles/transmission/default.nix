###: Sources:
# - <https://github.com/MayNiklas/nixos/blob/84d51b589dd1817266a726615c5038570f2beb6a/modules/transmission/default.nix>
{config, ...}: let
  cfg = config.services.transmission;
in {
  networking.firewall.allowedTCPPorts = [cfg.settings.rpc-port];

  systemd.services.transmission.serviceConfig = {
    Restart = "always";
    RestartSec = 10;
  };

  services.transmission = {
    enable = true;
    openFirewall = true;

    settings = {
      cache-size-mb = 32;
      download-dir = "/mnt/local/downloads/torrents/completed";
      incomplete-dir = "/mnt/local/downloads/torrents/.incomplete";
      incomplete-dir-enabled = false;
      message-level = 1;
      peer-port = 51413;
      peer-port-random-high = 65535;
      peer-port-random-low = 49152;
      peer-port-random-on-start = false;
      script-torrent-done-enabled = false;
      script-torrent-done-filename = null;
      umask = 2;
      watch-dir = "/mnt/local/downloads/torrents/watch";
      watch-dir-enabled = true;

      dht-enabled = false;
      encryption = 1;
      idle-seeding-limit-enabled = false;
      lpd-enabled = false;
      max-peers-global = 256;
      utp-enabled = false;
      peer-limit-global = 256; # what's the difference between this and max-peers-global?
      peer-limit-per-torrent = 100;
      pex-enabled = false;
      port-forwarding-enabled = false;
      preallocation = 1;
      prefetch-enabled = true;
      ratio-limit-enabled = false;
      rename-partial-files = true;

      rpc-bind-address = "0.0.0.0";
      rpc-port = 9093;
      rpc-whitelist-enabled = true;
      rpc-whitelist = "127.0.0.1,192.168.*.*";

      speed-limit-down = 50000;
      speed-limit-down-enabled = true;
      speed-limit-up = 15000;
      speed-limit-up-enabled = true;

      alt-speed-down = 12500;
      alt-speed-up = 1000;
      alt-speed-time-begin = 480;
      alt-speed-time-end = 120;
      alt-speed-time-day = 127;
      alt-speed-time-enabled = false;
    };
  };
}
