flake@{
  ...
}:
let
  inherit (flake.config.dotfield) meta;
in
{
  dotfield.hosts.nixos.boschic =
    { config, ... }:
    {
      sops.secrets."users/seadoom/hashed-password".neededForUsers = true;

      users.users.seadoom = {
        uid = 1000;
        isNormalUser = true;
        hashedPasswordFile = config.sops.secrets."users/seadoom/hashed-password".path;
        openssh.authorizedKeys.keys = meta.users.cdom.keys.ssh;
      };
    };
}
