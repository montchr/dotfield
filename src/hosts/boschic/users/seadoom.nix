flake@{
  ...
}:
let
  inherit (flake.config.dotfield) meta;
in
{
  dotfield.hosts.nixos.boschic.users.seadoom = {
    aspects = flake.config.dotfield.hosts.nixos.boschic.aspects ++ ([
      flake.config.dotfield.aspects.git__with-gpg-signing
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
