{config, ...}: {
  sops.secrets."users/zortflower/passphrase".neededForUsers = true;
  users.users.zortflower = {
    uid = 1001;
    isNormalUser = true;
    passwordFile = config.sops.secrets."users/zortflower/passphrase".path;
    extraGroups = [
      "video"
      "networkmanager"
    ];
  };
  home-manager.users.zortflower = hmArgs: {
    imports = hmArgs.roles.gui;
    home.stateVersion = "22.05";
  };
}
