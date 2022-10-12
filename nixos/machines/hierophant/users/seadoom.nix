{
  config,
  lib,
  pkgs,
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
    imports = with hmArgs.roles; remote ++ developer ++ trusted;

    home.stateVersion = "21.11";
  };
}
