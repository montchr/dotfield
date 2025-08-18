{ config, ... }:
{
  dotfield.hosts.nixos.ryosuke = {
    users.cdom = {
      features =
        config.dotfield.hosts.nixos.ryosuke.features
        ++ (with config.dotfield.features; [
          git__with-gpg-signing
          gpg__with-ssh
        ])
        ++ (with config.dotfield.users.cdom.features; [
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
