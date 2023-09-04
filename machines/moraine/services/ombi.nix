{config, ...}: let
  cfg = config.services.ombi;
in {
  services.ombi = {
    enable = true;
    openFirewall = true;
    port = 5081;
  };
  services.nginx.virtualHosts."requests.stormobservatory.com" = {
    # enableACME = true;
    # addSSL = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${builtins.toString cfg.port}";
    };
  };
}
