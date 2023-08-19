{
  flake,
  config,
  ...
}: let
  inherit (config.sops) secrets;
  l = flake.inputs.nixpkgs.lib // builtins;
  cfg = config.services.keycloak;
  hostname = "auth.seadome.net";
  httpUrl = "http://${cfg.settings.http-host}:${builtins.toString cfg.settings.http-port}";
in {
  services.keycloak = {
    enable = true;
    database.passwordFile = secrets."keycloak/db-password".path;
    database.type = "postgresql";
    initialAdminPassword = "veryconfusing";
    settings = {
      inherit hostname;
      proxy = "edge";
      http-host = "127.0.0.1";
      http-port = 8037;
      https-port = 8038;
      features = l.concatStringsSep "," [
        "account2"
        "admin2"
        "authorization"
        "client-policies"
        "impersonation"
        "web-authn"
      ];
    };
    sslCertificate = secrets."keycloak/cert".path;
    sslCertificateKey = secrets."keycloak/cert-key".path;
  };

  sops.secrets."keycloak/cert" = {};
  sops.secrets."keycloak/cert-key" = {};
  sops.secrets."keycloak/db-password".restartUnits = ["keycloak.service"];

  services.nginx.virtualHosts."${hostname}" = {
    enableACME = true;
    forceSSL = true;
    locations."/".proxyPass = httpUrl;
  };
}
