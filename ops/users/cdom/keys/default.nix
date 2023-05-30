{
  keys,
  metadata,
  ...
}: let
  inherit (metadata) hosts;
in
  [keys.ssh."0x135EEDD0F71934F3"]
  ++ (with hosts;
    hierophant.keys
    ++ hierophant.users.hierophant.keys
    ++ boschic.users.seadoom.keys
    ++ hodgepodge.users.seadoom.keys
    ++ aerattum.users.blink.keys
    ++ aerattum.users.workingcopy.keys
    ++ brakhage.users.blink.keys)
