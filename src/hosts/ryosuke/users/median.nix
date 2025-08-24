{ config, ... }:
{
  dotfield.hosts.nixos.ryosuke = {
    users.median = {
      aspects = config.dotfield.hosts.nixos.ryosuke.aspects;
      home.home.stateVersion = "24.05";
    };

    nixos = {
      users.users.median = {
        uid = 1001;
        isNormalUser = true;
        # TODO: generate
        # openssh.authorizedKeys.keys = ops.users.${username}.keys.default;
      };
    };
  };
}
