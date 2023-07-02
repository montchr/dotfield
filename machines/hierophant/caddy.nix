{
  config,
  ops,
  ...
}: let
  inherit (ops.metadata) networks;
  cfg = config.services.caddy;
in {
  services.caddy.enable = true;
  services.caddy.email = networks.seadome.contact;

  # Allow requesting HTTPS certs via Tailscale.
  services.tailscale.permitCertUid = cfg.user;
}
