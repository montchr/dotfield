{ inputs, ... }:

let
  inherit (inputs) haumea;

  hostName = "boschic";
  keys = haumea.lib.load {
    src = ./keys;
    loader = [ (haumea.lib.matchers.always (_: builtins.readFile)) ];
  };
in

{
  dotfield.meta.hosts.${hostName} = {
    age = keys."boschic.age";
    ipv4.address = "192.168.1.214";
    keys = [
      keys.boschic
      keys.boschic-rsa
    ];
    network = "home";
    networks.ts = "100.112.94.38";
    # users.seadoom = {
    #   age = keys.seadoom-at-boschic;
    #   keys = [ keys.ssh.seadoom-at-boschic ];
    # };
    syncthing.id = "5TCUNJM-PVGGNJ6-DETAT3O-PSMTOEP-SXRT7FP-62EFNZY-6ENFIYZ-3J2VHQJ";
  };
}
