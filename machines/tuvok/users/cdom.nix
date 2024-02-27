{
  config,
  ops,
  pkgs,
  ...
}: let
  username = "cdom";
in {
  sops.secrets."user-${username}-hashed-password".neededForUsers = true;

  users.users.${username} = {
    isNormalUser = true;
    hashedPasswordFile = config.sops.secrets."user-${username}-hashed-password".path;
    openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
    shell = pkgs.zsh;
  };

  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.displayManager.autoLogin.user = username;

  home-manager.users.${username} = hmArgs: {
    imports = with hmArgs.roles; workstation;
    home.stateVersion = "23.05";
  };
}
