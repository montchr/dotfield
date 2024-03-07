{ config, ... }:
{
  services.matrix-synapse.settings.oidc_providers = [
    {
      idp_id = "auth0";
      idp_name = "Auth0";
      issuer = "https://seadome.us.auth0.com";
      client_id = "HD7lKslGjMx7EEZqobkOAqPEztPFSkY9";
      scopes = [
        "openid"
        "profile"
      ];
      user_mapping_provider.config = {
        localpart_template = "{{ user.preferred_username }}";
        display_name_template = "{{ user.name }}";
      };
    }
  ];
  services.matrix-synapse.extraConfigFiles = [
    # FIXME: this does not correspond to a real settings -- must be in oidc providers, not top-level
    # (config.sops.secrets."matrix-synapse/client-secret-yaml".path)
  ];
}
