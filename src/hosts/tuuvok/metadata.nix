{ config, ... }:
let
  inherit (config.meta) hosts keys;
in
{
  meta.hosts.tuuvok = {
    hardware = {
      # We share a body...
      inherit (hosts.tuvix.hardware) mem vcpus;
      # ...different minds, same brain.
      system = "aarch64-linux";
    };
    keys = {
      age = keys.age.tuuvok;
      ssh = [
        keys.ssh.tuuvok
        keys.ssh.tuuvok-rsa
      ];
    };
    networks.ts = {
      ipv4 = "100.89.80.26";
      ipv6 = "fd7a:115c:a1e0::1c01:501a";
    };
    users.cdom.keys = {
      age = keys.age.cdom-at-tuuvok;
      ssh = [ keys.ssh.cdom-at-tuuvok ];
    };
    syncthing.id = "TR3RHZG-CZX3C6D-N2SDPVS-RI2H4JR-DEAVMKT-O7V4US2-LQK5WNR-V2TN2AA";
  };
}
