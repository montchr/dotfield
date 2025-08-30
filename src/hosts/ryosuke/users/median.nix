{ config, ... }:
let
  username = "median";
in
{
  users.users.${username} = {
    uid = 1001;
    isNormalUser = true;
    # TODO: generate
    # openssh.authorizedKeys.keys = flake.config.meta.users.${username}.keys.ssh;
  };

  home-manager.users.${username} = _: {
    imports = [ ../../../../home/mixins/graphical.nix ];
    home.stateVersion = "24.05";
  };
}
