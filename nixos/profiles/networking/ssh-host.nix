{
  lib,
  primaryUser,
  ...
}: {
  services.openssh = {
    enable = lib.mkForce true;
    passwordAuthentication = false;
    permitRootLogin = lib.mkDefault "no";
  };

  # Passwordless sudo when SSH'ing with keys
  security.pam.enableSSHAgentAuth = true;

  # FIXME: too open!!! set per-host explicitly.
  users.users.root.openssh.authorizedKeys.keys = primaryUser.authorizedKeys;
}
