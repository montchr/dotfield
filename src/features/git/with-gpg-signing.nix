flake@{ ... }:

{
  dotfield.aspects.git__with-gpg-signing.home =
    { config, ... }:
    let
      inherit (flake.config.dotfield.meta.users.${config.home.username}) whoami;
    in
    {
      programs.git.signing = {
        key = whoami.pgp.id;
        signByDefault = true;
      };
    };
}
