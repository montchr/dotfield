{ config, ... }:
{
  dotfield.hosts.nixos.ryosuke = {
    users.cdom = {
      features =
        config.dotfield.hosts.nixos.ryosuke.features
        ++ (with config.dotfield.features; [
          gpg

          "git/with-gpg-signing"
          "gpg/with-ssh-support"
        ]);
      home.home.stateVersion = "22.05";
    };

    nixos =
      let
        username = "cdom";
      in
      {
        users.users.${username} = {
          uid = 1000;
          isNormalUser = true;
          openssh.authorizedKeys.keys = config.dotfield.meta.users.cdom.keys.ssh;
        };
      };
  };

}
