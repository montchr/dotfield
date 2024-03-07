{ config, ... }:
let
  cfg = config.services.prometheus;
in
{
  services.prometheus = {
    enable = true;
    port = 9001;
    exporters = {
      node = {
        enable = true;
        enabledCollectors = [ "systemd" ];
        port = 9002;
      };
    };
    scrapeConfigs = [
      {
        job_name = config.networking.hostName;
        static_configs = [ { targets = [ "127.0.0.1:${builtins.toString cfg.exporters.node.port}" ]; } ];
      }
    ];
  };
}
