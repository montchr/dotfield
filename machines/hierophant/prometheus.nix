{config, ...}: let
  cfg = config.services.prometheus;
in {
  services.prometheus = {
    enable = true;
    port = 9001;
  };

  services.prometheus.scrapeConfigs = [
    {
      job_name = config.networking.hostName;
      static_configs = [
        {
          targets = ["127.0.0.1:${toString cfg.exporters.node.port}"];
        }
      ];
    }
  ];

  services.prometheus.exporters = {
    node = {
      enable = true;
      # <https://github.com/prometheus/node_exporter#collectors>
      enabledCollectors = ["processes" "systemd"];
      port = 9002;
    };

    # TODO: configure
    # nginx = {
    #   enable = true;
    #   port = 9003;
    # };
  };
}
