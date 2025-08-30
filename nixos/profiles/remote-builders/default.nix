{ flake, config, ... }:
{
  nix.distributedBuilds = true;

  lib.dotfield.mkBuildMachine = config.lib.dotfield.mkBuildMachineFor config.networking.hostName;
  lib.dotfield.mkBuildMachineFor =
    localHostName: builderHostName:
    let
      localVcpus = flake.config.meta.hosts.${localHostName}.hardware.vcpus or 1;
      builder = flake.config.meta.hosts.${builderHostName}.hardware or { };
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
