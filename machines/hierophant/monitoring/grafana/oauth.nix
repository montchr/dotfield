{
  ops,
  config,
  ...
}: let
  inherit (ops.services.keycloak.realms.default) urls;
  inherit (config.sops) secrets;
in {
  # <https://grafana.com/docs/grafana/latest/setup-grafana/configure-security/configure-authentication/keycloak/>
  # <https://github.com/NixOS/nixpkgs/pull/191768#discussion_r1002901711>
  services.grafana.settings."auth.generic_oauth" = {
    enabled = true;
    name = "Keycloak";
    allow_sign_up = true;
    client_id = "grafana-oauth";
    api_url = "${urls.oidc}/userinfo";
    auth_url = "${urls.oidc}/auth";
    token_url = "${urls.oidc}/token";
    scopes = "openid email profile offline_access roles";
    email_attribute_path = "email";
    login_attribute_path = "username";
    name_attribute_path = "full_name";
    role_attribute_path = "contains(roles[*], 'admin') && 'Admin' || contains(roles[*], 'editor') && 'Editor' || 'Viewer'";
  };

  # Provide client secret by environment variable.
  # <https://grafana.com/docs/grafana/latest/setup-grafana/configure-grafana/#override-configuration-with-environment-variables>
  systemd.services.grafana.serviceConfig.EnvironmentFile = secrets."grafana/env-vars".path;
  sops.secrets."grafana/env-vars".restartUnits = ["grafana.service"];
}
