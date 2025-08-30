{
  pkgs,
  config,
  ...
}:
{
  sops.secrets."users/seadoom/hashed-password".neededForUsers = true;

  users.users.seadoom = {
    uid = 1000;
    isNormalUser = true;
    hashedPasswordFile = config.sops.secrets."users/seadoom/hashed-password".path;
    openssh.authorizedKeys.keys = flake.config.meta.users.cdom.keys.ssh;
  };

  home-manager.users.seadoom = import ../../../../users/cdom/seadoom-at-boschic.nix;
}
