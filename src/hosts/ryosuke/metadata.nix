{ inputs, ... }:

let
  inherit (inputs) haumea;

  hostName = "ryosuke";
  keys = haumea.lib.load {
    src = ./keys;
    loader = [ (haumea.lib.matchers.always (_: builtins.readFile)) ];
  };
in

{
  dotfield.meta.hosts.${hostName} = {
    age = keys."ryosuke.age";
    ipv4.address = "192.168.1.217";
    hardware = {
      mem = 32;
      vcpus = 24;
      system = "x86_64-linux";
    };
    keys = [
      keys.ryosuke
      keys.ryosuke-rsa
    ];
    network = "home";
    networks.ts.ipv4.address = "100.123.41.68";
    # users.cdom = {
    #   age = keys.age.cdom-at-ryosuke;
    #   keys = [ keys.ssh.cdom-at-ryosuke ];
    # };
    syncthing.id = "2HDN7UF-5YKEBC7-4YB4L4H-A6Y7EGS-YZ5CSQX-AWWDKR7-KH5WIKH-D6LOTQ4";
  };
}
