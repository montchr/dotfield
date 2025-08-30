{
  meta.hosts.synoxyn = {
    ipv4.address = "192.168.1.197";
    network = "home";
    networks.ts = {
      ipv4.address = "100.106.113.40";
      ipv6.address = "fd7a:115c:a1e0::7601:7129";
    };
    users.cdom = {
      keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGd0Ao5dHAF1+uOanojoMaE6XBkzsa6ooMNe+rBNY5PI cdom@synoxyn"
      ];
    };
    syncthing.id = "QRAOJJT-LNPMO55-EE6GHCF-QQARBFR-RFH67IA-I465ZJQ-N37LLBU-BWZ2IAR";
  };
}
