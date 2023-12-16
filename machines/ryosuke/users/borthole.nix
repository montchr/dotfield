{
  config,
  ops,
  ...
}: let
  username = "borthole";
in {
  sops.secrets."users/${username}/passphrase".neededForUsers = true;

  users.users.${username} = {
    uid = 1000;
    isNormalUser = true;
    passwordFile = config.sops.secrets."users/${username}/passphrase".path;
    # TODO: generate
    # openssh.authorizedKeys.keys = ops.users.borthole.keys.default;
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
        profiles.spotify
        profiles.theme.fonts.monospace.iosevka-comfy
        profiles.theme.fonts.sans-serif.inter
        profiles.theme.fonts.serif.ibm-plex-serif
      ];
    home.stateVersion = "22.05";
  };
}
