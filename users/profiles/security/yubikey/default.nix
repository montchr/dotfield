{ config, lib, pkgs, ... }:

{
  # Without this configuration, Yubikey 5C will not work.
  # TODO: is this true? try again?
  my.hm.xdg.dataFile."gnupg/scdaemon.conf".text = ''
    reader-port Yubico Yubi
    disable-ccid
  '';
}
