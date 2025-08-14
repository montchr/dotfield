{ self, ... }:

{
  dotfield.features."git/with-gpg-signing".home =
    { config, ... }:
    let
      inherit (self.dotfield.meta.users.${config.home.username}) whoami;
    in
    {
      programs.git.signing = {
        key = whoami.pgp.id;
        signByDefault = true;
      };
    };
}
