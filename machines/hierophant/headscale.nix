##: Sources:
# - <https://github.com/Misterio77/nix-config/blob/10b7f73cbb039f5a77e71cff8950dcaa908086d6/hosts/alcyone/services/headscale.nix>
# - <https://carjorvaz.com/posts/setting-up-headscale-on-nixos/>
#
##: References:
# - <https://github.com/juanfont/headscale/blob/main/config-example.yaml>
# - <https://tailscale.com/kb/1054/dns/>
{
  ops,
  config,
  flake,
  ...
}: let
  inherit (flake.inputs.apparat.constants.networking) dns;
  inherit (ops.metadata) networks;
  inherit (networks.seadome) tailnet;
  cfg = config.services.headscale;
in {
  environment.systemPackages = [cfg.package];

  services.headscale = {
    enable = true;
    port = 8080; # default => 8080
    address = "127.0.0.1"; # default => "127.0.0.1"
    settings = {
      dns_config = {
        base_domain = tailnet.fqdn;
        # domains = [tailnet.fqdn];
        magic_dns = true;
        nameservers = dns.nameservers.quad9;
        override_local_dns = true;
      };
      server_url = tailnet.server.url;
      metrics_listen_addr = "127.0.0.1:8095";
      # These IP prefixes are the only valid values supported by the Tailscale client.
      # <https://github.com/juanfont/headscale/blob/36c9b5ce742d81eaea11c6b54c15fe5e60a33688/config-example.yaml#L58-L69>
      ip_prefixes = ["100.64.0.0/10" "fd7a:115c:a1e0::/48"];
      log.level = "info"; # default => "info"
      logtail.enabled = false;
      derp.server.enable = true;
      # Region ID must not collide with any others, otherwise this value will override the others.
      # Loaded by default: <https://controlplane.tailscale.com/derpmap/default>
      derp.server.region_id = 999;
    };
  };

  services.nginx.virtualHosts."link.seadome.net" = {
    forceSSL = true;
    enableACME = true;
    locations = {
      "/" = {
        proxyPass = "http://localhost:${toString cfg.port}";
        proxyWebsockets = true;
      };
      "/metrics" = {
        proxyPass = "http://${cfg.settings.metrics_listen_addr}/metrics";
      };
    };
  };
}
