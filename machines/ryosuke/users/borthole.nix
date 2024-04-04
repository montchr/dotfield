{ config, ... }:
let
  username = "borthole";
in
{
  sops.secrets."users/${username}/hashed-password".neededForUsers = true;

  users.users.${username} = {
    uid = 1000;
    isNormalUser = true;
    hashedPasswordFile = config.sops.secrets."users/${username}/hashed-password".path;
    # TODO: generate
    # openssh.authorizedKeys.keys = ops.users.borthole.keys.default;
  };

  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.displayManager.autoLogin.user = username;

  home-manager.users.${username} =
    { features, profiles, ... }:
    {
      imports = features.workstation ++ [ profiles.spotify ];
      home.stateVersion = "22.05";
    };
}
