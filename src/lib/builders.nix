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
      inherit (remoteMeta.hardware) system;
      hostName = remoteHostName;
      # FIXME: should be reversed, right?
      # speedFactor = builtins.ceil builderVcpus / localVcpus;
      speedFactor = builtins.ceil localCpus / remoteCpus;
      protocol = "ssh-ng";
      supportedFeatures = [
        "benchmark"
        "big-parallel"
      ];
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
          remoteHostName
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
