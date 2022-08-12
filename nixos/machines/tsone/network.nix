{
  config,
  lib,
  pkgs,
  peers,
  ...
}: let
  inherit (peers.hosts.tsone) ipv4 ipv6;
  interface = "eth0";
in {
  # Hetzner uses static IP assignments.
  networking.useDHCP = false;
  networking.usePredictableInterfaceNames = false;

  networking.defaultGateway = {
    inherit interface;
    address = ipv4.gateway;
  };
  networking.defaultGateway6 = {
    inherit interface;
    address = ipv6.gateway;
  };

  networking.interfaces.${interface} = {
    ipv4.addresses = [{inherit (ipv4) address prefixLength;}];
    ipv6.addresses = [
      {
        inherit (ipv6) prefixLength;
        address = "${ipv6.subnet}::1";
      }
    ];
  };
}
