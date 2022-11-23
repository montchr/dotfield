{
  config,
  primaryUser,
  ...
}: {
  sops.secrets."users/seadoom/passphrase".neededForUsers = true;
  users.users.seadoom = {
    uid = 1000;
    isNormalUser = true;
    passwordFile = config.sops.secrets."users/seadoom/passphrase".path;
    openssh.authorizedKeys.keys = primaryUser.authorizedKeys;
  };
  home-manager.users.seadoom = hmArgs: {
    imports = with hmArgs.roles; workstation;
    home.stateVersion = "21.11";
  };
}
