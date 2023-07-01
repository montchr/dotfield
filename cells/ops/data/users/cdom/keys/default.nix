{
  keys,
  metadata,
  ...
}: let
  inherit (metadata) hosts;
in
  [
    keys.ssh."0x135EEDD0F71934F3"
    keys.ssh.blink-at-phione1
  ]
  ++ hosts.boschic.users.seadoom.keys
  ++ hosts.brakhage.users.blink.keys
  ++ hosts.hodgepodge.users.seadoom.keys
  ++ hosts.tuvix.users.cdom.keys
