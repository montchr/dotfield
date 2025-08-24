{ self, lib, ... }:
{
  dotfield.aspects.jujutsu__with-gpg-signing.home =
    { whoami, ... }:
    {
      programs.jujutsu.settings.signing = {
        behavior = lib.mkDefault "own";
        backend = "gpg";
        key = whoami.pgp;
      };
    };
}
