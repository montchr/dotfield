{
  config,
  pkgs,
  ops,
  ...
}: let
  username = "seadoom";
in {
  sops.secrets."users/${username}/passphrase".neededForUsers = true;

  users.users.${username} = {
    uid = 1000;
    isNormalUser = true;
    passwordFile = config.sops.secrets."users/${username}/passphrase".path;
    openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
    shell = pkgs.zsh;
  };

  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.displayManager.autoLogin.user = username;

  home-manager.users.${username} = {
    profiles,
    roles,
    ...
  }: {
    imports =
      roles.workstation
      ++ [
        profiles.emacs.emacs-init
        profiles.shells.fish.default
        profiles.theme.fonts.monospace.iosevka-comfy
      ];
    home.stateVersion = "21.11";
  };
}
