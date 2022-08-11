{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [./ssh-host.nix];

  services.openssh.permitRootLogin = "prohibit-password";
  users.users.root.openssh.authorizedKeys.keys = import ../identity/authorized-keys.nix;
}
