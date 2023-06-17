rec {
  cubegarden.domain = "cube.garden";
  home = {
    domain = "home.arpa";
    ipv4 = {
      # TODO: rename to `gateway`
      address = "192.168.1.1";
      prefixLength = 24;
    };
  };
  local.domain = "test";
  loopgarden.domain = "loop.garden";
  seadome = {
    domain = "seadome.net";
    tailnet.fqdn = "link.${seadome.domain}";
    tailnet.server.url = "https://${seadome.tailnet.fqdn}";
  };
  tusecurewireless.domain = "temple.edu";
}
