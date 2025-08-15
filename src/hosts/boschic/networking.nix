flake@{ ... }:
let
  inherit (flake.config.dotfield.meta) hosts networks;
in
{
  dotfield.hosts.nixos.boschic.nixos = {
    # FIXME: no connection on boot -- i need to disable internet and re-enable
    # every time despite indication of a wired connection in GNOME status bar
    networking =
      let
        host = hosts.boschic;
        net = networks.${host.network};
        interface = "eth0";
      in
      {
        useDHCP = false;
        usePredictableInterfaceNames = false;
        # interfaces.wlp6s0.useDHCP = true;

        firewall = {
          enable = true;
          # allowedTCPPorts = [80 443];
        };

        defaultGateway = {
          inherit interface;
          inherit (net.ipv4) address;
        };

        interfaces.${interface} = {
          ipv4.addresses = [
            {
              inherit (host.ipv4) address;
              inherit (net.ipv4) prefixLength;
            }
          ];
        };
      };
  };
}
