{ self, ... }:
let
  hostName = "tuuvok";
in

{
  dotfield.meta.hosts.${hostName} = self.lib.host.mkMetadata hostName {
    hardware = {
      # We share a body...
      inherit (self.dotfield.meta.hosts.tuvix.hardware) mem vcpus;
      # ...different minds, same brain.
      system = "aarch64-linux";
    };
    networks.ts = {
      ipv4 = "100.89.80.26";
      ipv6 = "fd7a:115c:a1e0::1c01:501a";
    };
    # users.cdom = {
    #   age = keys.age.cdom-at-tuuvok;
    #   keys = [ keys.ssh.cdom-at-tuuvok ];
    # };
    syncthing.id = "TR3RHZG-CZX3C6D-N2SDPVS-RI2H4JR-DEAVMKT-O7V4US2-LQK5WNR-V2TN2AA";
  };
}
