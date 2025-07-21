let
  kdeconnectPorts = {
    from = 1714;
    to = 1764;
  };
in
{
  flake.modules.nixos.workstation = {
    networking.firewall = {
      allowedTCPPortRanges = [ kdeconnectPorts ];
      allowedUDPPortRanges = [ kdeconnectPorts ];
    };
  };
}
