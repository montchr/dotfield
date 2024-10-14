{ lib, ops, ... }:
let
  inherit (ops.hosts.platauc.hardware) mem vcpus;

  # Allow for some padding -- don't push usage to the limit.
  ramPad = 1;

  # TODO: cite source -- chromium needs at least 4GB, apparently, but that no
  # longer seems enough due to more frequent crashes during builds lately.
  requiredRamPerCore = 4 + ramPad;

  allowedTotalCores = mem / requiredRamPerCore;

  max-jobs = 1;
  cores = allowedTotalCores / max-jobs;
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
    ops.keys.ssh.cdom-at-tuuvok
    ops.keys.ssh.nixdaemon-at-ryosuke
    ops.keys.ssh.nixdaemon-at-tuuvok
  ];

  nix.settings = {
    inherit cores max-jobs;
    sandbox = true;
    connect-timeout = lib.mkForce 90;
    download-attempts = 10;
    show-trace = true;
  };
}
