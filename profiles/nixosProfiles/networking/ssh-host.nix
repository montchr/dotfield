{
  lib,
  ops,
  ...
}: {
  services.openssh = {
    enable = lib.mkForce true;
    settings.passwordAuthentication = false;
    settings.permitRootLogin = lib.mkDefault "no";
  };

  # Passwordless sudo when SSH'ing with keys
  security.pam.enableSSHAgentAuth = true;

  # FIXME: too open!!! set per-host explicitly.
  users.users.root.openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
}
