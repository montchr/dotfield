{ config, ... }:
{
  dotfield.hosts.nixos.ryosuke = {
    users.median = {
      features = config.dotfield.hosts.nixos.ryosuke.features;
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
