{
  config,
  ops,
  ...
}: let
  username = "cdom";
in {
  sops.secrets."users/${username}/passphrase".neededForUsers = true;

  users.users.${username} = {
    uid = 1000;
    isNormalUser = true;
    passwordFile = config.sops.secrets."users/${username}/passphrase".path;
    openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
  };

  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.displayManager.autoLogin.user = username;

  home-manager.users.${username} = hmArgs: {
    imports = with hmArgs.roles; workstation;
    home.stateVersion = "22.05";
  };
}
