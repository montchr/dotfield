{ ops, ... }:
let
  inherit (ops.hosts.platauc.hardware) ram vcpus;
  requiredRamPerCore = 4;
  allowedTotalCores = ram / requiredRamPerCore;

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
      assertion = cores * max-jobs * requiredRamPerCore <= ram;
      message = "Exceeded ${builtins.toString ram}GB RAM limit.";
    }
  ];

  roles.nix-remote-builder.schedulerPublicKeys = [
    ops.keys.ssh.cdom-at-tuvok
    ops.keys.ssh.nixdaemon-at-tuvok
  ];

  nix.settings = {
    inherit cores max-jobs;
    sandbox = true;
  };
}
