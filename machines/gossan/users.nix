{
  config,
  ops,
  ...
}: {
  dotfield.guardian.enable = true;
  dotfield.guardian.username = "cdom";
  users.mutableUsers = false;

  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = false;

  users.users.cdom = {
    isNormalUser = true;
    extraGroups = ["wheel"];
    passwordFile = config.sops.secrets.user-cdom-password.path;
    openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
  };

  home-manager.users.cdom = hmArgs: {
    imports = with hmArgs.roles; remote;
    home.stateVersion = "23.05";
  };
}