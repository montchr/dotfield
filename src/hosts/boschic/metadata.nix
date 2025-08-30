{ config, ... }:
let
  inherit (config.meta) keys;
in
{
  meta.hosts.boschic = {
    admins = [ "seadoom" ];
    ipv4.address = "192.168.1.214";
    keys = {
      age = keys.age.boschic;
      ssh = [
        keys.ssh.boschic
        keys.ssh.boschic-rsa
      ];
    };
    network = "home";
    networks.ts = "100.112.94.38";
    users.seadoom.keys = {
      age = keys.age.seadoom-at-boschic;
      ssh = [ keys.ssh.seadoom-at-boschic ];
    };
    syncthing.id = "5TCUNJM-PVGGNJ6-DETAT3O-PSMTOEP-SXRT7FP-62EFNZY-6ENFIYZ-3J2VHQJ";
  };
}
