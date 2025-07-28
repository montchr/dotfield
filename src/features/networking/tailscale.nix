{
  dotfield.nixos =
    { lib, config, ... }:
    lib.mkIf config.services.tailscale.enable {
      services.tailscale.interfaceName = "ts0";
      services.tailscale.useRoutingFeatures = "client";

      # Add MagicDNS nameserver, in addition to any existing nameservers.
      # <https://tailscale.com/kb/1063/install-nixos/#using-magicdns>
      networking.nameservers = [ "100.100.100.100" ];

      networking.firewall.trustedInterfaces = [ config.services.tailscale.interfaceName ];
      networking.firewall.allowedUDPPorts = [ config.services.tailscale.port ];
    };

}
