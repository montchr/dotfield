{ lib, ... }:
{
  flake.modules.nixos.network-service-discovery =
    { config, ... }:
    let
      cfg = config.services.avahi;
    in
    {
      # Network service discovery via multicast DNS (mDNS)
      services.avahi = {
        enable = true;
        nssmdns4 = true;
        openFirewall = cfg.enable;
        # TODO: maybe disable all by default? idk... why enable by
        # default anyway?
        publish = {
          enable = lib.mkDefault false;
          domain = true;
          userServices = true;
        };
      };

    };
}
