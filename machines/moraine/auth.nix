{
  # services.kanidm = {
  #   enableServer = true;
  #   serverSettings = {
  #     bindaddress = "[::]:8443"; # default => "127.0.0.1:8443"
  #     db_path = "/var/lib/kanidm/kanidm.db"; # default => "/var/lib/kanidm/kanidm.db"
  #   };
  # };

  services.oauth2_proxy = {
    enable = true;
    email.domains = ["*"];
    nginx.virtualHosts = ["test.storm.observer"];
    provider = "email";
    setXauthrequest = true;
    # deployed by colmena
    keyFile = "/run/keys/oauth2-proxy-env";
    extraConfig = {
      cookie-domain = ".storm.observer";
      skip-provider-button = true;
      whitelist-domain = ".storm.observer";
      set-authorization-header = true;
      # allowed-group = "/proxy";
      scope = "openid email profile";
    };
  };
}
