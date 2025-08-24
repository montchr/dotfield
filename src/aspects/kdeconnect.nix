let
  allowedPorts = {
    from = 1714;
    to = 1764;
  };
in
{
  dotfield.aspects.workstation.nixos = {
    networking.firewall = {
      allowedTCPPortRanges = [ allowedPorts ];
      allowedUDPPortRanges = [ allowedPorts ];
    };
  };

  dotfield.aspects.workstation.home = {
    services.kdeconnect.enable = true;
  };
}
