flake@{ self, ... }:
{
  hosts.nixos.tuuvok.configuration =
    {
      config,
      pkgs,
      ...
    }:
    let
      username = "cdom";
    in
    {
      sops.secrets."users/${username}/hashed-password".neededForUsers = true;

      users.users.${username} = {
        uid = 1000;
        isNormalUser = true;
        hashedPasswordFile = config.sops.secrets."users/${username}/hashed-password".path;
        openssh.authorizedKeys.keys = flake.config.meta.users.cdom.keys.ssh;
        shell = pkgs.bashInteractive;
      };

      home-manager.users.${username} = import (self.outPath + "/users/cdom/cdom-at-tuuvok.nix");
    };
}
