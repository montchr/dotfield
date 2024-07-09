{ ops, ... }:
{
  roles.nix-remote-builder.schedulerPublicKeys = ops.hosts.tuvok.keys ++ [
    ops.keys.ssh.cdom-at-tuvok
  ];
}
