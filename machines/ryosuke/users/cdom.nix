{ config, ops, ... }:
let
  username = "cdom";
in
{
  dotfield.guardian.enable = true;
  dotfield.guardian.username = "cdom";
  users.mutableUsers = false;

  sops.secrets."users/${username}/hashed-password".neededForUsers = true;

  users.users.${username} = {
    uid = 1000;
    isNormalUser = true;
    hashedPasswordFile = config.sops.secrets."users/${username}/hashed-password".path;
    openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
  };

  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.displayManager.autoLogin.user = username;

  home-manager.users.${username} =
    { features, profiles, ... }:
    {
      imports = features.workstation ++ [
        profiles.emacs.emacs-init
        # TODO: consider renaming these -- "default" is a little confusing
        # sometimes, e.g. in this case it could be interpreted as the default
        # user shell.
        #
        profiles.shells.fish.default
      ];
      home.stateVersion = "22.05";
    };
}
