{
  dotfield.users.cdom.features.development.home = {
    programs.jujutsu.settings.aliases = {
      # SYNOPSIS: jj harvest <revset>
      #
      # NOTE: `--from` revset argument is deliberately omitted a la
      # partial application so the argument may be provided in the
      # shell.  Surprisingly, this Just Works™!
      "harvest" = [
        "squash"
        "--interactive"
        "--to"
        "@"
        "--from"
      ];
      "l" = [
        "log"
        "--no-pager"
        "--limit=6"
      ];
      "s" = [
        "st"
        "--no-pager"
      ];
    };

  };
}
