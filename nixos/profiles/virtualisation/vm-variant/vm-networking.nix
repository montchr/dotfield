_: {
  virtualisation = {
    # N.B. If using the default SLiRP user networking, be aware that the guest's
    # firewall configuration must be configured to enable traffic through the
    # ports specified here.
    # TODO: remove or set sensible defaults
    # forwardPorts = lib.mkBefore [
    #   # Forward local port 2222 -> 22 to SSH into the VM via port 2222.
    #   # {
    #   #   from = "host";
    #   #   host.port = 2222;
    #   #   guest.port = 22;
    #   # }
    #   # Forward local port 80 -> 10.0.2.10:80 in the VLAN
    #   # {
    #   #   from = "guest";
    #   #   guest.address = "10.0.2.10";
    #   #   guest.port = 80;
    #   #   host.address = "127.0.0.1";
    #   #   host.port = 80;
    #   # }
    # ];
  };
}
