{
  aspects.workstation.nixos = {
    networking.firewall =
      let
        ports = {
          from = 1714;
          to = 1764;
        };
      in
      {
        allowedTCPPortRanges = [ ports ];
        allowedUDPPortRanges = [ ports ];
      };
  };

  aspects.workstation.home = {
    services.kdeconnect.enable = true;
    services.kdeconnect.indicator = true;
  };
}
