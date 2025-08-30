flake@{ self, ... }:
{
  hosts.nixos.hodgepodge.configuration =
    {
      config,
      pkgs,
      ...
    }:
    let
      username = "seadoom";
    in
    {
      sops.secrets."users/${username}/hashed-password".neededForUsers = true;

      users.users.${username} = {
        uid = 1000;
        isNormalUser = true;
        hashedPasswordFile = config.sops.secrets."users/${username}/hashed-password".path;
        openssh.authorizedKeys.keys = flake.config.meta.users.cdom.keys.ssh;
        extraGroups = [
          "audio"
          "video"
        ];
      };

      services.displayManager.autoLogin.enable = true;
      services.displayManager.autoLogin.user = username;

      home-manager.users.${username} = import (self.outPath + "/users/cdom/seadoom-at-hodgepodge.nix");
    };
}
