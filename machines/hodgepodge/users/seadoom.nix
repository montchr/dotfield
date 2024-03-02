{
  config,
  pkgs,
  ops,
  ...
}: let
  username = "seadoom";
  maybeGroups = builtins.filter (group: builtins.hasAttr group config.users.groups);
in {
  dotfield.guardian.enable = true;
  dotfield.guardian.username = "seadoom";
  users.mutableUsers = false;

  sops.secrets."users/${username}/passphrase".neededForUsers = true;

  users.users.${username} = {
    uid = 1000;
    isNormalUser = true;
    hashedPasswordFile = config.sops.secrets."users/${username}/passphrase".path;
    openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
    shell = pkgs.zsh;
    extraGroups = [
      "audio"
      "video"
    ];
  };

  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.displayManager.autoLogin.user = username;

  home-manager.users.${username} = {
    profiles,
    features,
    ...
  }: {
    imports =
      features.workstation
      ++ [
        profiles.emacs.emacs-init
        profiles.shells.fish.default
        profiles.spotify
        profiles.theme.fonts.monospace.iosevka-comfy
        profiles.qutebrowser
      ];
    home.stateVersion = "21.11";
  };
}
