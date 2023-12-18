{
  config,
  ops,
  ...
}: {
  sops.secrets."users/seadoom/passphrase".neededForUsers = true;
  users.users.seadoom = {
    uid = 1000;
    isNormalUser = true;
    hashedPasswordFile = config.sops.secrets."users/seadoom/passphrase".path;
    openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
  };
  home-manager.users.seadoom = hmArgs: {
    imports = with hmArgs.roles; workstation;
    home.stateVersion = "21.11";
  };
}
