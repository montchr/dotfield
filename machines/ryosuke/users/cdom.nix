{
  config,
  ops,
  ...
}: let
  username = "cdom";
in {
  sops.secrets."users/${username}/passphrase".neededForUsers = true;

  users.users.${username} = {
    uid = 1000;
    isNormalUser = true;
    passwordFile = config.sops.secrets."users/${username}/passphrase".path;
    openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
  };

  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.displayManager.autoLogin.user = username;

  home-manager.users.${username} = {
    roles,
    profiles,
    ...
  }: {
    imports =
      roles.workstation
      ++ [
        profiles.emacs.emacs-init
        profiles.shells.fish.default

        # profiles.theme.fonts.monospace.jetbrains-mono
        # profiles.theme.fonts.monospace.iosevka-xtal
        profiles.theme.fonts.monospace.iosevka-comfy
      ];
    home.stateVersion = "22.05";
  };
}
