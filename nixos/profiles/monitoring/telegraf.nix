{
  # <numtide/srvos>: "To use this module you also need to allow port 9273
  #                   either on the internet or on a vpn interface"
  networking.firewall.interfaces."eth0".allowedTCPPorts = [ 9273 ];
  # TODO(someday): configure vpn...
  # networking.firewall.interfaces."vpn0".allowedTCPPorts = [9273];
}
