let
  allowedPorts = {
    from = 1714;
    to = 1764;
  };
in
{
  dotfield.features.workstation.nixos = {
    networking.firewall = {
      allowedTCPPortRanges = [ allowedPorts ];
      allowedUDPPortRanges = [ allowedPorts ];
    };
  };

  dotfield.features.workstation.home = {
    services.kdeconnect.enable = true;
  };
}
