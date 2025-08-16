{
  dotfield.features.jujutsu__with-sign-on-push.home = {
    programs.jujutsu.settings = {
      # Defer signing until push to remote.  This provides a *very*
      # significant performance boost during jj operations using the git
      # backend due to the increased tendency towards mutation of the
      # underlying commits prior to push.
      git.sign-on-push = true;

      # Drop signatures on commit modification.  Commits will be re-signed
      # if necessary upon push.
      signing.behavior = "drop";
    };
  };
}
