{
  users.cdom.aspects.core.home = {
    programs.readline = {
      variables = {
        # Don't show hidden files unless a '.' is prefixed.
        match-hidden-files = false;

        # Improve completion usability.
        completion-ignore-case = true;
        completion-prefix-display-length = 2;
        completion-map-case = true;

        # Avoid pressing TAB so much.
        show-all-if-ambiguous = true;
        show-all-if-unmodified = true;

        # Indicate file types.
        visible-stats = true;

        # Disable the internal pager.
        page-completions = false;
      };
    };
  };
}
