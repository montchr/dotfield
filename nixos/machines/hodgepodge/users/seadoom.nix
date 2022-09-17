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
    uid = 1000;
    extraGroups = [
      "wheel"
      "seadome"
      "secrets"
      "keys"
      "video"
      "networkmanager"
    ];
    openssh.authorizedKeys.keys = primaryUser.authorizedKeys;
    shell = pkgs.zsh;
  };

  home-manager.users.seadoom = hmArgs: {
    imports = with hmArgs.roles; workstation;
  };
}
