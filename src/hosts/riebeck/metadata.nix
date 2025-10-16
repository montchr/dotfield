{ config, ... }:
let
  inherit (config.meta) hosts keys;
in
{
  meta.hosts.riebeck = {
    hardware = {
      mem = 32;
      vcpus = 14;
      system = "x86_64-linux";

    };
    supportedFeatures = [
      "nixos-test"
      "benchmark"
      "big-parallel"
      "kvm"
    ];
    keys = {
      age = keys.age.riebeck;
      ssh = [
        keys.ssh.riebeck
        keys.ssh.riebeck-rsa
      ];
    };
    users.cdom.keys = {
      #     age = keys.age.cdom-at-riebeck;
      ssh = [ keys.ssh.cdom-at-riebeck ];
    };
  };

}
