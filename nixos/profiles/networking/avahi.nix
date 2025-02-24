# Network service discovery via multicast DNS (mDNS)
# NOTE: Enable firewall explicitly in other profiles or machine configs.
{ lib, ... }:
{
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    publish = {
      enable = lib.mkDefault false;
      domain = true;
      userServices = true;
    };
  };
}
