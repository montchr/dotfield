{ config, ... }:
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

  home-manager.users.${username} = _: {
    imports = [ ../../../../home/mixins/graphical.nix ];
    home.stateVersion = "24.05";
  };
}
