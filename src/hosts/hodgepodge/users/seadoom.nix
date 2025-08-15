flake@{ ... }:
{
  dotfield.hosts.nixos.hodgepodge.users.seadoom = {
    features = flake.config.dotfield.hosts.hodgepodge.features ++ ([
      flake.config.dotfield.features."git/with-gpg-signing"
    ]);
    home.home.stateVersion = "21.11";
  };

  dotfield.hosts.nixos.hodgepodge.nixos =
    { config, ... }:
    let
      username = "seadoom";
    in
    {
      sops.secrets."users/${username}/hashed-password".neededForUsers = true;

      users.users.${username} = {
        uid = 1000;
        isNormalUser = true;
        hashedPasswordFile = config.sops.secrets."users/${username}/hashed-password".path;
        openssh.authorizedKeys.keys = flake.dotfield.meta.users.cdom.keys.ssh;
      };
    };
}
