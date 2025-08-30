{ flake, config, ... }:
let
  username = "cdom";
in
{
  users.users.${username} = {
    uid = 1000;
    isNormalUser = true;
    openssh.authorizedKeys.keys = flake.config.meta.users.cdom.keys.ssh;
  };

  home-manager.users.${username} = import ../../../../users/cdom/cdom-at-ryosuke.nix;
}
