{ config, ... }:
let
  inherit (builtins) ceil;
  inherit (config.dotfield.meta) hosts;

  mkRemoteBuilderFor =
    local: remote:
    let
      localVcpus = hosts.${local}.hardware.vcpus;
      remoteHardware = hosts.${remote}.hardware;
      remoteVcpus = remoteHardware.vcpus;
    in
    {
      inherit (remoteHardware) system;
      hostName = remote;

      # FIXME: should be reversed, right?
      # speedFactor = ceil remoteVcpus / localVcpus;
      speedFactor = ceil localVcpus / remoteVcpus;
      protocol = "ssh-ng";
      # maxJobs = 2;
      supportedFeatures = [
        "benchmark"
        "big-parallel"
      ];
    };
in
{
  flake.lib.remote-builders = {
    inherit mkRemoteBuilderFor;
  };
}
