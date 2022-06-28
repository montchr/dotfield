let
  inherit (builtins) fromTOML nameValuePair readFile;
  hosts = (fromTOML (readFile ../hosts.toml)).hosts;
  yubiGpg = readFile ./ssh-yubikey.pub;
in
with hosts;
[yubiGpg]
++ hierophant.keys
++ hierophant.users.hierophant.keys
++ boschic.users.seadoom.keys
++ hodgepodge.users.seadoom.keys
++ tapestone.users.seadoom.keys
++ aerattum.users.blink.keys
++ aerattum.users.workingcopy.keys
