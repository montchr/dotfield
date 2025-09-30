{
  users.cdom.aspects.development.home = {
    programs.jujutsu.settings = {
      aliases = {
        "examine" = [
          "log"
          "-T"
          "builtin_log_detailed"
          "-p"
          "-r"
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
      templates = {
        # FIXME: error │ 2: Value not found for ui.should-sign-off
        # draft_commit_description = ''
        #   concat(
        #     coalesce(description, default_commit_description, "\n"),
        #     if(
        #       config("ui.should-sign-off").as_boolean() && !description.contains("Signed-off-by: " ++ author.name()),
        #       "\nSigned-off-by: " ++ author.name() ++ " <" ++ author.email() ++ ">",
        #     ),
        #     "\n",
        #     surround(
        #       "\nJJ: This commit contains the following changes:\n", "",
        #       indent("JJ:     ", diff.summary()),
        #     ),
        #   )
        # '';
      };
      template-aliases = {
        # Display relative timestamps in log output
        "format_timestamp(timestamp)" = "timestamp.ago()";
      };
      ui = {
        movement.edit = true;
      };
    };
  };
}
