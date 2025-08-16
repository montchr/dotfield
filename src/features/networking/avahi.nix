### avahi :: Network service discovery via multicast DNS (mDNS)

# FIXME: Enable firewall
# NOTE: This must be enabled explicitly in host configurations.

{
  dotfield.features.networking__service-discovery.nixos = {
    services.avahi = {
      enable = true;
      nssmdns4 = true;
      publish = {
        domain = true;
        userServices = true;
      };
    };
  };
}
