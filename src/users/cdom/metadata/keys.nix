{ config, ... }:
let
  inherit (config.dotfield.meta) hosts keys;
in
{
  dotfield.meta.users.cdom.keys = [
    keys.ssh.cdom-yubikey-rsa
  ]
  ++ hosts.boschic.users.seadoom.keys.ssh
  ++ hosts.brakhage.users.blink.keys.ssh
  ++ hosts.hodgepodge.users.seadoom.keys.ssh
  ++ hosts.ryosuke.users.cdom.keys.ssh
  ++ hosts.tuvix.users.cdom.keys.ssh
  ++ hosts.tuuvok.users.cdom.keys.ssh;
}
