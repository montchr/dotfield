{ config, ops, ... }:
let
  username = "cdom";
in
{
  sops.secrets."users/cdom/hashed-password".neededForUsers = true;

  users.users.${username} = {
    uid = 1000;
    isNormalUser = true;
    hashedPasswordFile = config.sops.secrets."users/cdom/hashed-password".path;
    openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
  };

  home-manager.users.${username} = import ../../../users/cdom/cdom-at-ryosuke.nix;
}
