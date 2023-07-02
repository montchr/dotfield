{lib, ...}: {
  networking.useDHCP = lib.mkDefault true;

  # Allow this host to function as a Tailscale exit node.
  services.tailscale.useRoutingFeatures = "both";
}
