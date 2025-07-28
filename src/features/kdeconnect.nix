let
  allowedPorts = {
    from = 1714;
    to = 1764;
  };
in
{
  dotfield.modules.workstation.nixos = {
    networking.firewall = {
      allowedTCPPortRanges = [ allowedPorts ];
      allowedUDPPortRanges = [ allowedPorts ];
    };
  };

  dotfield.modules.workstation.home = {
    services.kdeconnect.enable = true;
  };
}
