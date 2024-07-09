{ config, ops, ... }:
{
  imports = [
    ./disk-config.nix
    ./buildings.nix
  ];

  dotfield.guardian.enable = true;
  dotfield.guardian.username = "cdom";

  users.mutableUsers = false;

  sops.defaultSopsFile = ./secrets/secrets.yaml;
  sops.secrets."users/cdom/hashed-password".neededForUsers = true;

  users.users.cdom = {
    uid = 1000;
    isNormalUser = true;
    hashedPasswordFile = config.sops.secrets."users/cdom/hashed-password".path;
    openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
  };

  system.stateVersion = "24.05";
}
