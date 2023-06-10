{
  config,
  ops,
  ...
}: {
  sops.secrets."users/seadoom/passphrase".neededForUsers = true;
  users.users.seadoom = {
    isNormalUser = true;
    extraGroups = ["wheel"];
    passwordFile = config.sops.secrets."users/seadoom/passphrase".path;
    openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
  };
  home-manager.users.seadoom = hmArgs: {
    imports = with hmArgs.roles; remote ++ developer ++ trusted;

    home.stateVersion = "21.11";
  };
}
