{
  config,
  pkgs,
  ops,
  ...
}:
let
  username = "seadoom";
in
{
  dotfield.guardian.enable = true;
  dotfield.guardian.username = "seadoom";
  users.mutableUsers = false;

  sops.secrets."users/${username}/hashed-password".neededForUsers = true;

  users.users.${username} = {
    uid = 1000;
    isNormalUser = true;
    hashedPasswordFile = config.sops.secrets."users/${username}/hashed-password".path;
    openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
    extraGroups = [
      "audio"
      "video"
    ];
  };

  services.displayManager.autoLogin.enable = true;
  services.displayManager.autoLogin.user = username;

  home-manager.users.${username} = import ../../../users/cdom/seadoom-at-hodgepodge.nix;
}
