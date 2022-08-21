{
  config,
  lib,
  pkgs,
  primaryUser,
  ...
}: {
  services.openssh.enable = lib.mkForce true;
  services.openssh.permitRootLogin = "prohibit-password";
  # FIXME: too open!!! set per-host explicitly.
  users.users.root.openssh.authorizedKeys.keys = primaryUser.authorizedKeys;
}
