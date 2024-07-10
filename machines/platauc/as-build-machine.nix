{ ops, ... }:
{
  roles.nix-remote-builder.schedulerPublicKeys = [
    ops.keys.ssh.cdom-at-tuvok
    ops.keys.ssh.nixdaemon-at-tuvok
  ];
}
