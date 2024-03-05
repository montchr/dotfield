{
  flake,
  ops,
  ...
}: let
  inherit (flake.inputs.apparat.constants.networking) dns;
in {
  networking.domain = ops.hosts.hierophant.domain;
  networking.nameservers = dns.nameservers.cloudflare;

  # Allow this host to function as a Tailscale exit node.
  services.tailscale.useRoutingFeatures = "both";
}
