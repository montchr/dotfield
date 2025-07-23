{
  # Defer signing until push to remote.  This provides a very
  # significant performance boost during iteration due to the
  # increased ephemerality of git commits using the git backend.
  programs.jujutsu.settings.git.sign-on-push = true;

  # Drop signatures on commit modification.  Commits will be re-signed
  # if necessary upon push.
  programs.jujutsu.settings.signing.behavior = "drop";
}
