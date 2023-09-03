{config, ...}: let
  inherit (config.users) users;
  inherit (config.sops) secrets;

  nginxCfg = config.services.nginx;
  nginxUser = users.${nginxCfg.user}.name;
  nginxGroup = users.${nginxCfg.user}.group;
  cfg = config.services.deluge;
in {
  services.deluge.web.enable = true;
  # Keep firewall closed -- use reverse proxy.
  services.deluge.web.openFirewall = false;
  # TODO: change from default?
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

  # FIXME: unable to start service -- maybe related: https://dietpi.com/forum/t/deluge-not-working-after-dietpi-update/5832/11
  services.nginx.virtualHosts."deluge.storm.observer" = {
    enableACME = true;
    forceSSL = true;

    # sslCertificate = secrets."services/deluge/ssl-cert".path;
    # sslCertificateKey = secrets."services/deluge/ssl-key".path;

    # TODO: re-enable -- commented-out as sanity check when unable to start service
    # kTLS = true;

    # FIXME: re-enable! do not leave web-ui running without auth
    # FIXME: does the `deluge` user/group need permission?
    # basicAuthFile = secrets."services/deluge/http-basic-auth".path;

    locations."/".proxyPass = "http://localhost:${toString cfg.web.port}";
  };
}
