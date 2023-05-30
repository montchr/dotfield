let
  inherit (peers) hosts;
  peers = import ../ops/metadata/peers.nix;
  hostKeys = builtins.mapAttrs (_n: v: v.keys) hosts;
  trustedUsers = import ./authorized-keys.nix;

  # servers = with hostKeys;
  #   hierophant;

  workstations = with hostKeys;
    boschic
    ++ hodgepodge
    ++ ryosuke;
  # allMachines = servers ++ workstations;
in {
  "espanso/personal.yml.age".publicKeys = workstations ++ trustedUsers;

  # FIXME
  # "espanso/work.yml.age".publicKeys = workstations ++ trustedUsers;
}
