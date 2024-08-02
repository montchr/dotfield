{
  config,
  lib,
  ops,
  ...
}:
let
  inherit (ops.hosts.platauc.hardware) mem vcpus;
  requiredRamPerCore = 4;
  allowedTotalCores = mem / requiredRamPerCore;

  cores = allowedTotalCores / max-jobs;
  max-jobs = 1;
in
{
  assertions = [
    {
      assertion = cores * max-jobs <= (vcpus - 1);
      message = "Total core usage (${
        builtins.toString (cores * max-jobs)
      }) must not be above the ${builtins.toString (vcpus - 1)} core threshold";
    }
    {
      assertion = cores * max-jobs * requiredRamPerCore <= mem;
      message = "Exceeded ${builtins.toString mem}GB RAM limit.";
    }
  ];

  roles.nix-remote-builder.schedulerPublicKeys = [
    ops.keys.ssh.cdom-at-tuvok
    ops.keys.ssh.nixdaemon-at-ryosuke
    ops.keys.ssh.nixdaemon-at-tuvok
  ];

  nix.settings = {
    inherit cores max-jobs;
    sandbox = true;
    connect-timeout = lib.mkForce 90;
    download-attempts = 10;
    show-trace = true;
  };
}
