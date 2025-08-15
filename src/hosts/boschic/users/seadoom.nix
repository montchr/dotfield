flake@{
  ...
}:
let
  inherit (flake.config.dotfield) meta;
in
{

  dotfield.hosts.nixos.boschic.users.seadoom = {
    features =
      flake.config.dotfield.hosts.boschic.features
      ++ (with flake.config.dotfield.features; [
        gpg
        "git/with-gpg-signing"
      ]);
    home = {
      home.stateVersion = "21.11";
    };
  };

  dotfield.hosts.nixos.boschic.nixos =
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
