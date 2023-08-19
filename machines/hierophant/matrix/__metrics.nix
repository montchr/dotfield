{flake, ...}: let
  l = flake.inputs.nixpkgs.lib // builtins;
  metricsPort = 9009;
  bindAddrs = ["::1" "127.0.0.1"];
  toTargetAddr = v: "${v}:${builtins.toString metricsPort}";
in {
  services.matrix-synapse.settings.enable_metrics = true;
  services.matrix-synapse.settings.listeners = l.singleton {
    port = metricsPort;
    type = "metrics";
    tls = false;
    bind_addresses = bindAddrs;
    resources = l.singleton {
      names = ["metrics"];
      compress = false;
    };
  };
  services.prometheus.scrapeConfigs = l.singleton {
    job_name = "matrix-synapse";
    scrape_interval = "15s";
    metrics_path = "/_synapse/metrics";
    static_configs = l.singleton {targets = l.map toTargetAddr bindAddrs;};
  };
}
