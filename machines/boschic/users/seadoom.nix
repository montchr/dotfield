{
  pkgs,
  config,
  ops,
  ...
}: {
  sops.secrets."users/seadoom/passphrase".neededForUsers = true;
  users.users.seadoom = {
    uid = 1000;
    isNormalUser = true;
    hashedPasswordFile = config.sops.secrets."users/seadoom/passphrase".path;
    openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
    shell = pkgs.zsh;
  };
  home-manager.users.seadoom = {
    profiles,
    roles,
    ...
  }: {
    imports =
      roles.workstation
      ++ [
        profiles.browsers.nyxt
        profiles.git.with-pgp-signing
      ];
    home.stateVersion = "21.11";
  };
}
