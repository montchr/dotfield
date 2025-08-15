{ config, ... }:
{
  dotfield.hosts.nixos.ryosuke = {
    users.median = {
      features = (with config.dotfield.features; [ workstation ]);
      home.home.stateVersion = "24.05";
    };

    nixos =
      let
        username = "median";
      in
      {
        users.users.${username} = {
          uid = 1001;
          isNormalUser = true;
          # TODO: generate
          # openssh.authorizedKeys.keys = ops.users.${username}.keys.default;
        };
      };
  };
}
