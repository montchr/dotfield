{ config, ... }:
let
  inherit (config.meta) hosts keys;
in
{
  users.cdom = { };

  meta.users.cdom = {
    keys.ssh = [
      keys.ssh.cdom-yubikey-rsa
    ]
    ++ hosts.boschic.users.seadoom.keys.ssh
    ++ hosts.brakhage.users.blink.keys.ssh
    ++ hosts.hodgepodge.users.seadoom.keys.ssh
    ++ hosts.ryosuke.users.cdom.keys.ssh
    ++ hosts.tuvix.users.cdom.keys.ssh
    ++ hosts.tuuvok.users.cdom.keys.ssh;
  };
}
