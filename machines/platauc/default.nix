{ config, ops, ... }:
{
  imports = [ ./disk-config.nix ];

  dotfield.guardian.enable = true;
  dotfield.guardian.username = "cdom";

  users.mutableUsers = false;

  sops.defaultSopsFile = ./secrets/secrets.yaml;
  sops.secrets."users/cdom/hashed-password".neededForUsers = true;

  users.users.cdom = {
    uid = 1000;
    isNormalUser = true;
    initialHashedPassword = "$y$j9T$oXblkpzc3Afple6/j2jZm/$RwdXc7ADcMDBwvpzzw2KXkj/nw5NX2ogbkB0w25OTe8";
    hashedPasswordFile = config.sops.secrets."users/cdom/hashed-password".path;
    openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
  };

  system.stateVersion = "24.05";
}
