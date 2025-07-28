let
  inherit (inputs) haumea;

  hostName = "hodgepodge";
  keys = haumea.lib.load {
    src = ./keys;
    loader = [ (haumea.lib.matchers.always (_: builtins.readFile)) ];
  };
in

{
  dotfield.meta.hosts.${hostName} = {
    age = keys."hodgepodge.age";
    ipv4.address = "192.168.1.152";
    keys = [ keys.hodgepodge ];
    network = "home";
    networks.ts = "100.71.240.35";
    # users.seadoom.keys = [ keys.ssh.seadoom-at-hodgepodge ];
    syncthing.id = "W7EFFEO-BAZIKPC-M5C2OOT-JXR6CIP-MISL4ID-2ZUBFYT-44ZEWUK-6R75OA3";
  };
}
