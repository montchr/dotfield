{
  inputs,
  lib,
  config,
  ...
}:
let
  inherit (inputs) dmerge;

  mkBuildMachineFor =
    localHostName: remoteHostName:
    let
      localCpus = config.meta.hosts.${localHostName}.hardware.vcpus or 1;
      remoteMeta = config.meta.hosts.${remoteHostName};
      remoteCpus = remoteMeta.hardware.vcpus or 1;
    in
    {
      inherit (remoteMeta) supportedFeatures;
      inherit (remoteMeta.hardware) system;
      hostName = remoteHostName;
      speedFactor = builtins.ceil remoteCpus / localCpus;
      protocol = "ssh-ng";
    };

  mkBuildMachineModuleFor =
    localHostName: remoteHostName:
    {
      extraHostNames ? [ ],
      maxJobs ? 2,
    }:
    let
      remoteMeta = config.meta.hosts.${remoteHostName};
    in
    {
      programs.ssh.knownHosts.${remoteHostName} = {
        publicKey = lib.head remoteMeta.keys.ssh;
        hostNames = [
          remoteMeta.ipv4.address
          remoteMeta.networks.ts.ipv4.address
          remoteMeta.networks.ts.ipv6.address
        ]
        ++ extraHostNames;
      };
      nix.buildMachines = [
        (dmerge.merge (mkBuildMachineFor localHostName remoteHostName) {
          inherit maxJobs;
        })
      ];
    };
in
{
  flake.lib.builders = {
    inherit
      mkBuildMachineFor
      mkBuildMachineModuleFor
      ;
  };
}
