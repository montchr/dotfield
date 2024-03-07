{ config, ... }:
{
  sops.secrets."users/zortflower/passphrase".neededForUsers = true;
  users.users.zortflower = {
    uid = 1001;
    isNormalUser = true;
    hashedPasswordFile = config.sops.secrets."users/zortflower/passphrase".path;
    extraGroups = [
      "video"
      "networkmanager"
    ];
  };
  home-manager.users.zortflower = hmArgs: {
    imports = with hmArgs.features; graphical;
    home.stateVersion = "22.05";
  };
}
