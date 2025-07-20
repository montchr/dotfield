{ config, ... }:
let
  inherit (config.dotfield) whoami;
in
{
  # Defer signing until push to remote.  This provides a very
  # significant performance boost during iteration due to the
  # increased ephemerality of git commits using the git backend.
  programs.jujutsu.settings.git.sign-on-push = true;

  programs.jujutsu.settings.signing = {
    behavior = "own";
    backend = "gpg";
    key = whoami.pgp;
  };
}
