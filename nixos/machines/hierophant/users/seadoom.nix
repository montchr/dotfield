{
  config,
  primaryUser,
  ...
}: {
  sops.secrets."users/seadoom/passphrase".neededForUsers = true;
  users.users.seadoom = {
    isNormalUser = true;
    extraGroups = ["wheel"];
    passwordFile = config.sops.secrets."users/seadoom/passphrase".path;
    openssh.authorizedKeys.keys = primaryUser.authorizedKeys;
  };
  home-manager.users.seadoom = hmArgs: {
    imports =
      hmArgs.roles.remote
      ++ hmArgs.roles.dev
      ++ hmArgs.roles.trusted;

    home.stateVersion = "21.11";
  };
}
