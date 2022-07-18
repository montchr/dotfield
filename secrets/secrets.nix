let
  inherit (peers) hosts;
  peers = import ../ops/metadata/peers.nix;
  hostKeys = builtins.mapAttrs (n: v: v.keys) hosts;
  trustedUsers = import ../identity/authorized-keys.nix;

  servers = with hostKeys;
    hierophant
    ++ tsone;

  workstations = with hostKeys;
    boschic
    ++ hodgepodge;

  allMachines = servers ++ workstations;
in {
  "wireless.env.age".publicKeys = workstations ++ trustedUsers;

  "aws/aws-cdom-default.pem.age".publicKeys = workstations ++ trustedUsers;

  "espanso/personal.yml.age".publicKeys = workstations ++ trustedUsers;
  # FIXME
  # "espanso/work.yml.age".publicKeys = workstations ++ trustedUsers;
}
