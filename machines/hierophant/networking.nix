{flake, ...}: let
  inherit (flake.inputs.apparat.constants.networking) dns;
in {
  networking.nameservers = dns.nameservers.cloudflare;

  # Allow this host to function as a Tailscale exit node.
  services.tailscale.useRoutingFeatures = "both";
}
