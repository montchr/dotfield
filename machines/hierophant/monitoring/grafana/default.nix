{config, ...}: let
  inherit (cfg.settings) server;
  cfg = config.services.grafana;
in {
  services.grafana.enable = true;
  services.grafana.settings = {
    server = {
      enforce_domain = true;
      enable_gzip = true;
      domain = "grafana.seadome.net";
      http_port = 3000;
      http_addr = "127.0.0.1";
      root_url = "https://${server.domain}/";
    };
  };

  services.nginx.virtualHosts."${server.domain}" = {
    enableACME = true;
    addSSL = true;
    locations."/" = {
      proxyPass = "http://${server.http_addr}:${builtins.toString server.http_port}";
      proxyWebsockets = true;
    };
  };
}
