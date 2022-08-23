{
  config,
  lib,
  pkgs,
  primaryUser,
  ...
}: {
  services.openssh = {
    enable = lib.mkForce true;
    passwordAuthentication = false;
    permitRootLogin = lib.mkDefault "no";
  };

  # FIXME: too open!!! set per-host explicitly.
  users.users.root.openssh.authorizedKeys.keys = primaryUser.authorizedKeys;
}
