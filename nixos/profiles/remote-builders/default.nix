{ config, ops, ... }:
{
  nix.distributedBuilds = true;

  lib.dotfield.mkBuildMachine = config.lib.dotfield.mkBuildMachineFor config.networking.hostName;
  lib.dotfield.mkBuildMachineFor =
    localHostName: builderHostName:
    let
      localVcpus = ops.hosts.${localHostName}.hardware.vcpus or 1;
      builder = ops.hosts.${builderHostName}.hardware or { };
      builderVcpus = builder.vcpus or 1;
    in
    {
      inherit (builder) system;
      hostName = builderHostName;

      # TODO: should be reversed, right?
      # speedFactor = builtins.ceil builderVcpus / localVcpus;
      speedFactor = builtins.ceil localVcpus / builderVcpus;

      protocol = "ssh-ng";
      # maxJobs = 2;
      supportedFeatures = [
        "benchmark"
        "big-parallel"
      ];
    };
}
