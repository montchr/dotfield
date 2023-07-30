###: Sources:
# - <https://nixos.org/manual/nixos/stable/index.html#module-services-matrix-synapse>
{
  pkgs,
  config,
  ops,
  ...
}: let
  inherit (ops.metadata.networks.loopgarden) domain;
  fqdn = "synapse.${domain}";
  clientConfig."m.homeserver".base_url = "https://${fqdn}";
  serverConfig."m.server" = "${fqdn}:443";
  mkWellKnown = data: ''
    add_header Content-Type application/json;
    add_header Access-Control-Allow-Origin *;
    return 200 '${builtins.toJSON data}';
  '';
in {
  imports = [
    ./__secrets.nix
    # Provides required well-known data.
    # ../seadome-dot-net.nix
  ];

  services.matrix-synapse = {
    enable = true;
    settings = {
      server_name = domain;
      # enable_registration = true;
      # enable_registration_captcha = true;
      # FIXME: enable! needs smtp config. tho auth0 should be able to handle this...
      # registrations_require_3pid = ["email"];
      # FIXME: enable! disabled for testing.
      # if you, a visitor, managed to register, congrats, but i'm deleting your account.
      registration_requires_token = true;
      # recaptcha_public_key = "6Ld3EGsnAAAAAHNqASQIhCuTkzazE0DqfwBBc3PD";
      default_identity_server = domain;
      listeners = [
        {
          port = 8008;
          bind_addresses = ["::1"];
          type = "http";
          tls = false;
          x_forwarded = true;
          resources = [
            {
              names = ["client" "federation"];
              compress = true;
            }
          ];
        }
        # FIXME: when this is active, error due to no resources
        #   stderr) error: The option `services.matrix-synapse.settings.listeners."[definition 1-entry 2]".resources' is used but not defined.
        # {
        #   port = 8009;
        #   type = "metrics";
        #   bind_addresses = ["::1" "127.0.0.1"];
        # }
      ];
      # oidc_providers = [
      #   {
      #     idp_id = "auth0";
      #     idp_name = "Auth0";
      #     issuer = "https://seadome.us.auth0.com";
      #     client_id = "HD7lKslGjMx7EEZqobkOAqPEztPFSkY9";
      #     scopes = ["openid" "profile"];
      #     user_mapping_provider.config = {
      #       localpart_template = "{{ user.preferred_username }}";
      #       display_name_template = "{{ user.name }}";
      #     };
      #   }
      # ];
    };
  };

  services.matrix-synapse.extraConfigFiles = [
    # FIXME: needs update after domain change
    # (config.sops.secrets."matrix-synapse/recaptcha-private-key-yaml".path)
    (config.sops.secrets."matrix-synapse/registration-shared-secret-yaml".path)
    # FIXME: this does not correspond to a real settings -- must be in oidc providers, not top-level
    # (config.sops.secrets."matrix-synapse/client-secret-yaml".path)
  ];

  # NOTE: Database created manually.
  services.postgresql.enable = true;

  services.nginx.virtualHosts."element.${domain}" = {
    enableACME = true;
    forceSSL = true;
    serverAliases = [
      "chat.${domain}"
    ];

    root = pkgs.element-web.override {
      conf = {
        default_server_config = clientConfig;
      };
    };
  };

  services.nginx.virtualHosts."${domain}" = {
    enableACME = true;
    forceSSL = true;
    # Declare the real server FQDN at the base domain.
    # The server location differs from `server_name`.
    # <https://matrix-org.github.io/synapse/latest/delegate.html>
    locations."= /.well-known/matrix/server".extraConfig = mkWellKnown serverConfig;
    # Homeserver discovery from clients.
    # https://spec.matrix.org/latest/client-server-api/#getwell-knownmatrixclient
    locations."= /.well-known/matrix/client".extraConfig = mkWellKnown clientConfig;
  };

  services.nginx.virtualHosts."${fqdn}" = {
    enableACME = true;
    forceSSL = true;
    # The server has no user-facing HTML to display, as Synapse only provides an
    # API. This does not necessarily need to return a 404 -- could do a 301
    # redirect instead. A Matrix client webapp *should not* live on the same
    # domain, to avoid XSS attacks.
    locations."/".extraConfig = ''
      return 404;
    '';

    # Forward all Matrix API calls to the synapse Matrix homeserver.
    # A trailing slash *must not* be used here.
    locations."/_matrix".proxyPass = "http://[::1]:8008";

    # Forward requests for e.g. SSO and password-resets.
    locations."/_synapse/client".proxyPass = "http://[::1]:8008";
  };
}
