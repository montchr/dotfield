{ops, ...}: let
  username = "anomich";
in {
  users.mutableUsers = false;
  users.users.${username} = {
    isNormalUser = true;
    extraGroups = ["wheel"];
    openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
  };
  home-manager.users.${username} = hmArgs: {
    imports = hmArgs.roles.remote;
    home.stateVersion = "23.05";
  };
}
