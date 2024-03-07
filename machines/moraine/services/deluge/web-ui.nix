{ config, ... }:
let
  inherit (config.users) users;
  inherit (config.sops) secrets;

  nginxCfg = config.services.nginx;
  nginxUser = users.${nginxCfg.user}.name;
  nginxGroup = users.${nginxCfg.user}.group;
  cfg = config.services.deluge;
in
{
  services.deluge.web.enable = true;
  # Keep firewall closed -- use reverse proxy.
  services.deluge.web.openFirewall = false;
  services.deluge.web.port = 8112; # default

  sops.secrets."services/deluge/ssl-cert" = {
    owner = nginxUser;
    group = nginxGroup;
  };

  sops.secrets."services/deluge/ssl-key" = {
    owner = nginxUser;
    group = nginxGroup;
  };

  sops.secrets."services/deluge/http-basic-auth" = {
    owner = nginxUser;
    group = nginxGroup;
  };

  services.nginx.virtualHosts."deluge.storm.observer" = {
    enableACME = true;
    forceSSL = true;
    basicAuthFile = secrets."services/deluge/http-basic-auth".path;
    locations."/".proxyPass = "http://localhost:${toString cfg.web.port}";
  };
}
