{ config, ... }:
let
  cfg = config.services.prometheus;
  scrape = exporter: {
    job_name = exporter;
    static_configs = [ { targets = [ "127.0.0.1:${toString cfg.exporters.${exporter}.port}" ]; } ];
  };
in
{
  services.prometheus = {
    enable = true;
    port = 9001;
  };

  services.prometheus.scrapeConfigs = [
    {
      job_name = config.networking.hostName;
      static_configs = [ { targets = [ "127.0.0.1:${toString cfg.exporters.node.port}" ]; } ];
    }
    (scrape "nginx")
    (scrape "nginxlog")
    (scrape "postgres")
  ];

  services.prometheus.exporters = {
    node = {
      enable = true;
      # <https://github.com/prometheus/node_exporter#collectors>
      enabledCollectors = [
        "processes"
        "systemd"
      ];
    };

    nginx.enable = true;
    nginxlog.enable = true;

    postgres.enable = true;
    postgres.runAsLocalSuperUser = true;
    postgres.extraFlags = [ "--auto-discover-databases" ];
  };

  services.nginx.statusPage = true;
}
