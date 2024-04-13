{ root, ... }:
let
  inherit (root) hosts keys;
in
[ keys.ssh."0x135EEDD0F71934F3" ]
++ hosts.boschic.users.seadoom.keys
++ hosts.brakhage.users.blink.keys
++ hosts.hodgepodge.users.seadoom.keys
++ hosts.ryosuke.users.cdom.keys
++ hosts.tuvix.users.cdom.keys
++ hosts.tuvok.users.cdom.keys
