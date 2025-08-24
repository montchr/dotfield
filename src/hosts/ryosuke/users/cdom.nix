{ config, ... }:
{
  dotfield.hosts.nixos.ryosuke = {
    users.cdom = {
      aspects =
        config.dotfield.hosts.nixos.ryosuke.aspects
        ++ (with config.dotfield.aspects; [
          git__with-gpg-signing
          gpg__with-ssh
        ])
        ++ (with config.dotfield.users.cdom.aspects; [
          jobwork
        ]);

      home.home.stateVersion = "22.05";
    };

    nixos = {
      users.users.cdom = {
        uid = 1000;
        isNormalUser = true;
        openssh.authorizedKeys.keys = config.dotfield.meta.users.cdom.keys.ssh;
      };
    };
  };

}
