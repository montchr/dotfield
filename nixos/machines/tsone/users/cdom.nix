{
  config,
  primaryUser,
  ...
}: let
  inherit (primaryUser) authorizedKeys;
in {
  sops.secrets."users/cdom/passphrase".neededForUsers = true;
  users.mutableUsers = false;
  users.users.cdom = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = ["wheel"];
    passwordFile = config.sops.secrets."users/cdom/passphrase".path;
    openssh.authorizedKeys.keys = authorizedKeys;
  };
  home-manager.users.cdom = hmArgs: {
    imports = with hmArgs.roles; remote;
    home.stateVersion = "22.05";
  };
}
