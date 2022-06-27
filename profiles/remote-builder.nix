{ config, lib, pkgs, ... }:

{
  services.openssh.permitRootLogin = lib.mkForce "prohibit-password";
  users.users.root.openssh.authorizedKeys.keys = import ../secrets/authorized-keys.nix;

  services.openssh.enable = true;
  services.openssh.hostKeys = [
    {
      bits = 4096;
      path = "/etc/ssh/ssh_host_rsa_key";
      type = "rsa";
    }
    {
      path = "/etc/ssh/ssh_host_ed25519_key";
      type = "ed25519";
    }
  ];
}
