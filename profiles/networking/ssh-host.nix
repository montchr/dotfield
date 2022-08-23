{
  config,
  lib,
  pkgs,
  primaryUser,
  outputs,
  ...
}: let
  hosts = builtins.attrNames outputs.nixosConfigurations;
in {
  services.openssh = {
    enable = lib.mkForce true;
    passwordAuthentication = false;
    permitRootLogin = lib.mkDefault "no";
  };

  programs.ssh = {
    # Each hosts public key
    knownHostsFiles = lib.flatten (map
      (host: [
        ../../${host}/ssh_host_ed25519_key.pub
        ../../${host}/ssh_host_rsa_key.pub
      ])
      hosts);
  };

  # Passwordless sudo when SSH'ing with keys
  security.pam.enableSSHAgentAuth = true;

  # FIXME: too open!!! set per-host explicitly.
  users.users.root.openssh.authorizedKeys.keys = primaryUser.authorizedKeys;
}
## sources:
# - https://git.sr.ht/~misterio/nix-config/blame/62878319554637d4b3d76539f0416e7756743ea8/hosts/common/global/openssh.nix
