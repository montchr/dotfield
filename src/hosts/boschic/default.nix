{ lib, config, ... }:
let
  inherit (config.dotfield.meta) keys;
  lib' = config.lib;
  hostName = "boschic";
in
{
  dotfield.hosts.nixos.${hostName} = {
    modules = [ ];

    users.groups.wheel.members = [ "seadoom" ];
  };

  dotfield.meta.hosts.${hostName} = {
    admins = [ "seadoom" ];
    ipv4.address = "192.168.1.214";
    keys = {
      age = keys.age.${hostName};
      ssh = [
        keys.ssh.${hostName}
        keys.ssh."${hostName}-rsa"
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
