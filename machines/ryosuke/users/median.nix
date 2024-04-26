{ config, ... }:
let
  username = "median";
in
{
  sops.secrets."users/median/hashed-password".neededForBoot = true;

  users.users.${username} = {
    uid = 1001;
    isNormalUser = true;
    hashedPasswordFile = config.sops.secrets."users/median/hashed-password".path;
    # TODO: generate
    # openssh.authorizedKeys.keys = ops.users.${username}.keys.default;
  };

  home-manager.users.${username} =
    { features, profiles, ... }:
    {
      imports = features.graphical ++ [ ];
      home.stateVersion = "24.05";
    };
}
