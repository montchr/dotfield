{ config, ... }:
let
  username = "median";
in
{
  sops.secrets."users/${username}/hashed-password".neededForUsers = true;

  users.users.${username} = {
    uid = 1001;
    isNormalUser = true;
    hashedPasswordFile = config.sops.secrets."users/${username}/hashed-password".path;
    # TODO: generate
    # openssh.authorizedKeys.keys = ops.users.${username}.keys.default;
  };

  services.displayManager.autoLogin.enable = true;
  services.displayManager.autoLogin.user = username;

  home-manager.users.${username} =
    { features, profiles, ... }:
    {
      imports = features.graphical ++ [ ];
      home.stateVersion = "24.05";
    };
}
