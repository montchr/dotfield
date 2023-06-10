{
  config,
  ops,
  ...
}: {
  sops.secrets."users/cdom/passphrase".neededForUsers = true;
  users.mutableUsers = false;
  users.users.cdom = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = ["wheel"];
    passwordFile = config.sops.secrets."users/cdom/passphrase".path;
    openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
  };
  home-manager.users.cdom = hmArgs: {
    imports = hmArgs.roles.remote;
    home.stateVersion = "23.05";
  };
}
