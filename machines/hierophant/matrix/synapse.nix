{
  flake,
  pkgs,
  config,
  ops,
  ...
}: let
  inherit (ops.networks.loopgarden) domain;
  inherit (flake.perSystem) packages;
  l = flake.inputs.nixpkgs.lib // builtins;
  cfg = config.services.matrix-synapse;
  fqdn = "matrix.${domain}";
  elementFqdn = "element.${domain}";
  baseUrl = "https://${fqdn}";
  clientConfig."m.homeserver".base_url = baseUrl;
  serverConfig."m.server" = "${fqdn}:443";
  mkWellKnown = data: ''
    add_header Content-Type application/json;
    add_header Access-Control-Allow-Origin *;
    return 200 '${builtins.toJSON data}';
  '';
in {
  imports = [
    ./__metrics.nix
    ./__secrets.nix
    # ./__idp-oidc.nix
  ];

  environment.systemPackages = [packages.synadm];

  services.borgbackup.jobs."services-backup".paths = [cfg.dataDir];

  services.matrix-synapse = {
    enable = true;
    settings = {
      server_name = domain;
      public_baseurl = baseUrl;
      enable_registration = true;
      enable_registration_captcha = true;
      registrations_require_3pid = ["email"];
      registration_requires_token = true;
      recaptcha_public_key = "6LcdyoAnAAAAADxF-7vQjWoF8jp0U0pkf3wgGrkD";
      admin_contact = "support@matrix.loop.garden";
      auto_join_rooms = [
        "#general:loop.garden"
      ];
      listeners = l.singleton {
        port = 8008;
        bind_addresses = ["::1" "127.0.0.1"];
        type = "http";
        tls = false;
        x_forwarded = true;
        resources = l.singleton {
          names = ["client" "federation"];
          compress = false;
        };
      };
    };
  };

  services.matrix-synapse.extraConfigFiles = [
    (config.sops.secrets."matrix-synapse/email-config-yaml".path)
    (config.sops.secrets."matrix-synapse/recaptcha-private-key-yaml".path)
    (config.sops.secrets."matrix-synapse/registration-shared-secret-yaml".path)
  ];

  # NOTE: Database created manually.
  services.postgresql.enable = true;

  services.nginx.virtualHosts.${elementFqdn} = {
    enableACME = true;
    forceSSL = true;
    serverAliases = ["chat.${domain}"];

    root = pkgs.element-web.override {
      conf = {
        default_server_config = clientConfig;
        default_country_code = "US";
        show_labs_settings = true;
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
