{ inputs, ... }:
let
  inherit (inputs.apparat.lib.net.constants) dns;
in
{
  aspects.core.nixos = {
    networking.nameservers = dns.nameservers.quad9;
    networking.networkmanager.insertNameservers = dns.nameservers.quad9;
  };
}
