{
  pkgs,
  config,
  ops,
  ...
}:
{
  sops.secrets."users/seadoom/hashed-password".neededForUsers = true;

  users.users.seadoom = {
    uid = 1000;
    isNormalUser = true;
    hashedPasswordFile = config.sops.secrets."users/seadoom/hashed-password".path;
    openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
    shell = pkgs.zsh;
  };

  home-manager.users.seadoom =
    { profiles, features, ... }:
    {
      imports = features.workstation ++ [
        profiles.git.with-pgp-signing
        # XXX: nyxt build failure
        # profiles.browsers.nyxt
      ];
      home.stateVersion = "21.11";
    };
}
