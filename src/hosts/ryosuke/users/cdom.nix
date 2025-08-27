{ config, ops, ... }:
let
  username = "cdom";
in
{
  users.users.${username} = {
    uid = 1000;
    isNormalUser = true;
    openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
  };

  home-manager.users.${username} = import ../../../../users/cdom/cdom-at-ryosuke.nix;
}
