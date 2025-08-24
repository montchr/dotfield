flake@{ ... }:
{
  dotfield.hosts.nixos.boschic.users.zortflower = {
    aspects = [ flake.config.dotfield.aspects.workstation ];
    home.home.stateVersion = "22.05";
  };

  dotfield.hosts.nixos.boschic.nixos =
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
}
