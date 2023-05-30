let
  inherit (builtins) readFile;
  inherit (peers) hosts;
  peers = import ../ops/metadata/peers.nix;
  yubiGpg = readFile ./keys/id_rsa_gpg_yk.pub;
in
  with hosts;
    [yubiGpg]
    ++ hierophant.keys
    ++ hierophant.users.hierophant.keys
    ++ boschic.users.seadoom.keys
    ++ hodgepodge.users.seadoom.keys
    ++ aerattum.users.blink.keys
    ++ aerattum.users.workingcopy.keys
    ++ brakhage.users.blink.keys
