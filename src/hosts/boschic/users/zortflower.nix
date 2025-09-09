flake@{ self, ... }:
{
  hosts.nixos.boschic = {
    configuration =
      { config, ... }:
      {
        sops.secrets."users/zortflower/hashed-password".neededForUsers = true;

        users.users.zortflower = {
          uid = 1001;
          isNormalUser = true;
          hashedPasswordFile = config.sops.secrets."users/zortflower/hashed-password".path;
          extraGroups = [
            "video"
            "networkmanager"
          ];
        };
      };

    users.zortflower = {
      configuration = {
        home.stateVersion = "22.05";
      };
    };
  };
}
