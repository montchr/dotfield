{
  config,
  ops,
  ...
}: let
  username = "anomich";
in {
  sops.secrets.user-anomich-password.neededForUsers = true;

  users.users.${username} = {
    isNormalUser = true;
    extraGroups = ["wheel"];
    passwordFile = config.sops.secrets."user-${username}-password".path;
    openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
  };

  home-manager.users.${username} = hmArgs: {
    home.stateVersion = "23.05";
  };
}
