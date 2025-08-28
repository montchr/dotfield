# Network service discovery via multicast DNS (mDNS)
# NOTE: Enable firewall explicitly in other profiles or machine configs.
{
  services.avahi = {
    enable = true;
    nssmdns4 = true;
  };
}
