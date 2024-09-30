{
  config,
  ops,
  pkgs,
  ...
}:
let
  username = "cdom";
in
{
#  sops.secrets."users/${username}/hashed-password".neededForUsers = true;

  users.users.${username} = {
    uid = 1000;
    isNormalUser = true;
#    hashedPasswordFile = config.sops.secrets."users/${username}/hashed-password".path;
    openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
    shell = pkgs.bashInteractive;
  };

  home-manager.users.${username} = import ../../../users/cdom/cdom-at-tuuvok.nix;
}
