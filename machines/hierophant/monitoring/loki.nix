{config, ...}: {
  services.loki = {
    enable = true;
    configFile = ./loki-local-config.yaml;
  };
}
