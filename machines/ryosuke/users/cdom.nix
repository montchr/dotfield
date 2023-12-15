{
  config,
  ops,
  ...
}: let
  username = "cdom";
in {
  dotfield.guardian.enable = true;
  dotfield.guardian.username = "cdom";
  users.mutableUsers = false;

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
        # TODO: consider renaming these -- "default" is a little confusing
        # sometimes, e.g. in this case it could be interpreted as the default
        # user shell.
        #
        profiles.shells.fish.default
        profiles.theme.fonts.monospace.iosevka-comfy
      ];
    home.stateVersion = "22.05";
  };
}
