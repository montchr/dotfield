{
  config,
  ops,
  ...
}: let
  username = "cdom";
in {
  sops.secrets."user-${username}-password".neededForUsers = true;

  users.users.${username} = {
    isNormalUser = true;
    hashedPasswordFile = config.sops.secrets."user-${username}-password".path;
    openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
  };

  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.displayManager.autoLogin.user = username;

  home-manager.users.${username} = hmArgs: {
    imports = with hmArgs.roles; workstation;
    home.stateVersion = "23.05";
  };
}
