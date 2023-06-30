let
  inherit ((import ../ops).metadata) hosts;
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
}
