{
  services.samba = {
    enable = true;
    securityType = "user";
    extraConfig = ''
      workgroup = WORKGROUP
      server string = smbnix
      netbios name = smbnix
      security = user
      hosts allow = 192.168.5.0/24
      hosts deny = 0.0.0.0/0
      guest account = nobody
      map to guest = bad user
    '';
    shares = {
      public = {
        # TODO: double check path
        path = "/var/lib/transmission/Downloads";
        "read only" = true;
        browseable = "yes";
        "guest ok" = "yes";
        "create mask" = "0644";
        "directory mask" = "0755";
        "force user" = "root";
        "force group" = "root";
        comment = "Public samba share.";
      };
    };
  };

  networking.firewall = {
    # FIXME
    allowedTCPPorts = [139 445 cfg.web-port];
    allowedUDPPorts = [137 138];
  };
}
