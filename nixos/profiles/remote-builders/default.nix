{ config, ops, ... }:
let
  inherit (config.networking) hostName;
  inherit (ops.hosts.${hostName}.hardware) vcpus;

in

{
  imports = [ ./platauc.nix ];

  nix.distributedBuilds = true;

  lib.dotfield.mkBuildMachine =
    machine:
    let
      hw = ops.hosts.${machine}.hardware;
    in
    {
      inherit (hw) system;
      hostName = machine;
      speedFactor = builtins.ceil vcpus / hw.vcpus;
      protocol = "ssh-ng";
      # maxJobs = 2;
      supportedFeatures = [
        "benchmark"
        "big-parallel"
      ];
    };
}
