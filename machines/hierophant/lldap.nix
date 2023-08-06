{config, ...}: let
  inherit (config.sops) secrets;
  inherit (config) users;
  cfg = config.services.lldap;

  fqdn = "auth.seadome.net";
in {
  services.lldap = {
    enable = true;
    settings = {
      ldap_base_dn = "dc=seadome,dc=net";
      ldap_user_email = "ops@seadome.net";
      http_url = "https://${fqdn}";
    };
    environment = {
      "LLDAP_JWT_SECRET_FILE" = secrets."lldap/jwt_secret".path;
      "LLDAP_LDAP_USER_PASS_FILE" = secrets."lldap/ldap_user_pass".path;
    };
    environmentFile = secrets."lldap/environment_file".path;
  };

  services.nginx.virtualHosts."${fqdn}" = {
    enableACME = true;
    addSSL = true;
    locations."/" = {
      proxyPass = "http://${cfg.settings.http_host}:${builtins.toString cfg.settings.http_port}";
    };
  };

  # FIXME: user does not exist -- probably need to fork the nixos module
  sops.secrets = {
    "lldap/environment_file" = {
      owner = users.lldap.name;
      group = users.lldap.group;
      mode = "0440";
      restartUnits = ["lldap.service"];
    };
    "lldap/jwt_secret" = {
      owner = users.lldap.name;
      group = users.lldap.group;
      mode = "0440";
      restartUnits = ["lldap.service"];
    };
    "lldap/ldap_user_pass" = {
      owner = users.lldap.name;
      group = users.lldap.group;
      mode = "0440";
      restartUnits = ["lldap.service"];
    };
  };
}
